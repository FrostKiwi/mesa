#include "nir.h"
#include "nir_builder.h"

typedef enum block_type {
   BLOCK_TYPE_BLOCK,
   BLOCK_TYPE_IF,
   BLOCK_TYPE_LOOP,
} block_type;

static nir_block *
follow_until_block(nir_block *start, nir_block *end)
{
   assert(start && end);

   struct set *d = _mesa_pointer_set_create(NULL);
   struct set *n = _mesa_pointer_set_create(NULL);

   nir_block *res = NULL;

   _mesa_set_add(n, start);
   while (n->entries) {
      struct set_entry *e = _mesa_set_random_entry(n, NULL);
      nir_block *cur = (nir_block*)e->key;

      if (cur->successors[0] == end ||
          cur->successors[1] == end) {
         res = cur;
         break;
      }

      _mesa_set_add(d, cur);
      _mesa_set_remove(n, e);
      if (cur->successors[0] && !_mesa_set_search(d, cur->successors[0]))
         _mesa_set_add(n, cur->successors[0]);
      if (cur->successors[1] && !_mesa_set_search(d, cur->successors[1]))
         _mesa_set_add(n, cur->successors[1]);
   }

   _mesa_set_destroy(d, NULL);
   _mesa_set_destroy(n, NULL);

   return res;
}

static nir_cf_node *
follow_until_function(nir_cf_node *node)
{
   while (node->parent->type != nir_cf_node_function) {
      switch (node->parent->type) {
      case nir_cf_node_loop: {
         // nir_loop *loop = nir_cf_node_as_loop(node->parent);
         // TODO asserts
         node = node->parent;
         break;
      }
      default:
         unreachable("");
      }
   }
   return node;
}

static nir_block *
follow_until_multiple_preds(nir_block *block)
{
   /* follow blocks with one sucessor until we hit one with multiple
    * predecessors */
   while (block) {
      /* if a successor is deper in the CF tree, skip to the end of that
       * node */
      switch (block->cf_node.parent->type) {
      case nir_cf_node_if: {
         nir_if *nif = nir_cf_node_as_if(block->cf_node.parent);
         block = nir_if_last_then_block(nif)->successors[0];
         break;
      }
      case nir_cf_node_loop: {
         nir_loop *loop = nir_cf_node_as_loop(block->cf_node.parent);
         block = nir_cf_node_as_block(nir_cf_node_next(&loop->cf_node));
         break;
      }
      case nir_cf_node_function:
         /* if we found a block with multiple predecessors, at least one
          * of those have to be on the top level of a function */
         if (block->predecessors->entries > 1) {
            set_foreach(block->predecessors, entry) {
               const nir_block *b = entry->key;
               if (b->cf_node.parent->type == nir_cf_node_function)
                  return block;
            }
         }
         /* if we have two successors, we give up if they have different
          * parents or the common parent is a function. This can only happen
          * in a block with a goto_if instruction, which also means this
          * block needs to be handled before the currently handled one */
         if (block->successors[1]) {
            if (block->successors[0]->cf_node.parent !=
                block->successors[1]->cf_node.parent)
               return NULL;
            if (block->successors[0]->cf_node.parent->type == nir_cf_node_function)
               return NULL;
         }
         block = block->successors[0];
         break;
      default:
         unreachable("unhandled CF type");
      }
   }

   return block;
}

/* for the given block, what type of cf node can we create out of it.
 * BLOCK_TYPE_BLOCK means we can't figure out anything meaningful */
static block_type
check_block(nir_block *block)
{
   assert(block);
   if (!block->successors[1])
      return BLOCK_TYPE_BLOCK;

   for (unsigned i = 0; i < 2; ++i) {
      nir_block *suc = block->successors[i];
      if (!suc || suc->cf_node.parent->type == nir_cf_node_function) {
         continue;
      }

      // now we have to check if the successor is the entry of the cf node
      if (suc->cf_node.parent->parent->type != nir_cf_node_function)
         return BLOCK_TYPE_BLOCK;

      if (suc->cf_node.parent->type != nir_cf_node_loop ||
          nir_loop_first_block(nir_cf_node_as_loop(suc->cf_node.parent)) != suc)
         return BLOCK_TYPE_BLOCK;
   }

   /* the main trick here is to follow each paths until we hit a block with
    * multiple predecessors and see how those blocks relate to each other */
   nir_block *t = follow_until_multiple_preds(block->successors[0]);
   nir_block *e = follow_until_multiple_preds(block->successors[1]);

   if (!e && !t)
      return BLOCK_TYPE_BLOCK;

   /* if we follow both successors and the first block with multiple
    * predecessors is the same on both paths, we can create an if node */
   if (e == t)
      return BLOCK_TYPE_IF;

   /* if one of the paths followed ends up with the initial block, we can
    * create a loop node */
   if (e == block || t == block) {
      /* if both paths end up in the same block we are completly screwed */
      assert(e != t);
      return BLOCK_TYPE_LOOP;
   }

   return BLOCK_TYPE_BLOCK;
}

static void
unlink_blocks(nir_block *a, nir_block *b)
{
   if (a->successors[0] == b) {
      a->successors[0] = a->successors[1];
      a->successors[1] = NULL;
   }
   else if (a->successors[1] == b) {
      a->successors[0] = NULL;
   }
   _mesa_set_remove_key(b->predecessors, a);
}

static void
link_blocks(nir_block *a, nir_block *b)
{
   if (!a->successors[0]) {
      a->successors[0] = b;
   }
   else {
      a->successors[1] = b;
   }
   _mesa_set_add(b->predecessors, a);
}

static bool
extract_blocks(nir_cf_list *cf_list, nir_block *head, nir_block *branch, nir_block *merge)
{
   nir_block *split = follow_until_block(branch, merge);

   if (split) {
      nir_cf_extract(cf_list,
                     nir_before_cf_node(follow_until_function(&branch->cf_node)),
                     nir_after_cf_node(follow_until_function(&split->cf_node)));
      return true;
   } else {
      return false;
   }
}

static bool
lower_block(nir_builder *b, nir_block *block)
{
   block_type type = check_block(block);
   if (type == BLOCK_TYPE_BLOCK)
      return false;

   nir_instr *instr = nir_block_last_instr(block);
   assert(instr->type == nir_instr_type_jump);
   nir_jump_instr *jump = nir_instr_as_jump(instr);
   assert(jump->type == nir_jump_goto_if);

   nir_block *e = block->successors[0];
   nir_block *t = block->successors[1];

   switch (type) {
   case BLOCK_TYPE_IF: {
      bool t_split, e_split;
      nir_cf_list cflt, cfle;
      nir_block *merge = follow_until_multiple_preds(e);

      assert(merge->predecessors->entries >= 2);
      b->cursor = nir_after_block(block);

      t_split = extract_blocks(&cflt, block, t, merge);
      e_split = extract_blocks(&cfle, block, e, merge);

      nir_src cond = jump->condition;
      nir_instr_remove(instr);

      nir_if *nif = nir_push_if_src(b, cond);
      if (t_split)
         nir_cf_reinsert(&cflt, b->cursor);
      nir_push_else(b, nif);
      if (e_split)
         nir_cf_reinsert(&cfle, b->cursor);

      nir_pop_if(b, nif);
      break;
   }
   case BLOCK_TYPE_LOOP: {
      nir_cf_list cfl;
      nir_block *head = e == block ? e : t;
      /* TODO: handle loop heads with multiple predecessors */
      assert(block->predecessors->entries == 2);
      b->cursor = nir_before_block(head);

      nir_block *entry = nir_block_cf_tree_prev(head);
      assert(_mesa_set_search(head->predecessors, entry));
      b->cursor = nir_after_block(entry);

      nir_src cond = jump->condition;
      nir_instr_remove(instr);

      nir_cf_extract(&cfl, nir_before_cf_node(&head->cf_node), nir_after_cf_node(&block->cf_node));
      nir_loop *nloop = nir_push_loop(b);
      nir_cf_reinsert(&cfl, b->cursor);

      /* cursor broke, so we need to reset it */
      b->cursor = nir_after_block(nir_loop_last_block(nloop));

      nir_if *nif = nir_push_if_src(b, cond);
      nir_jump(b, head == e ? nir_jump_break : nir_jump_continue);
      nir_push_else(b, nif);
      nir_jump(b, head == t ? nir_jump_break : nir_jump_continue);
      nir_pop_if(b, nif);

      nir_pop_loop(b, nloop);
      break;
   }
   default:
      unreachable("");
      return false;
   }

   return true;
}

static bool
lower_impl(nir_function_impl *impl)
{
   nir_builder b;
   nir_builder_init(&b, impl);

   bool final_progress = false;
   bool progress;

   do {
      progress = false;
      for (nir_cf_node *node = &nir_start_block(impl)->cf_node;
           node; node = nir_cf_node_next(node)) {
         if (node->type != nir_cf_node_block)
            continue;

         if (lower_block(&b, nir_cf_node_as_block(node))) {
            nir_metadata_preserve(impl, nir_metadata_none);
            nir_repair_ssa_impl(impl);
            progress = true;
            final_progress = true;
            break;
         }
      }
   } while (progress);

   return final_progress;
}

static void
post_cleanup(nir_block *block)
{
   if (nir_block_last_instr(block))
      return;
   if (block->cf_node.parent->type != nir_cf_node_function)
      return;

   nir_block *suc = block->successors[0];
   unlink_blocks(block, suc);

   set_foreach(block->predecessors, entry) {
      nir_block *other = (nir_block*)entry->key;
      unlink_blocks(other, block);

      if (suc)
         link_blocks(other, suc);
   }

   exec_node_remove(&block->cf_node.node);
}

bool
nir_lower_goto_ifs(nir_shader *shader)
{
   bool progress = false;

   nir_foreach_function(function, shader) {
      if (function->impl)
         progress |= lower_impl(function->impl);
   }

   shader->structured = true;
   nir_foreach_function(function, shader) {
      nir_foreach_block_safe(block, function->impl) {
         if (nir_start_block(function->impl) == block)
            continue;
         post_cleanup(block);
         if (!nir_block_ends_in_jump(block))
            continue;
         nir_jump_instr *jump = nir_instr_as_jump(nir_block_last_instr(block));
         if (jump->type != nir_jump_goto_if)
            continue;

         shader->structured = false;
         break;
      }
      if (!shader->structured)
         break;
   }

   return progress;
}
