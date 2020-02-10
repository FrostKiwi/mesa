#include "nir.h"
#include "nir_builder.h"

struct path {
   struct set *reachable;
   struct path_fork *fork;
};

struct path_fork {
   bool is_var;
   union {
      nir_variable *path_var;
      nir_ssa_def *path_ssa;
   };
   struct path pathes[2];
};

struct routes {
   struct path regular;
   struct path brk;
   struct path cont;
   struct routes *loop_backup;
};

struct strct_lvl {
   struct exec_node node;
   struct set *blocks;
   struct path out_path;
   struct set *reach;
   bool skip_start;
   bool skip_end;
   bool irreducible;
};

static void
set_path_vars(nir_builder *b, struct path_fork *fork, nir_block *target)
{
   int i;
   while (fork) {
      for (i = 0; i < 2; i++) {
         if (_mesa_set_search(fork->pathes[i].reachable, target)) {
            if (fork->is_var)
               nir_store_var(b, fork->path_var, nir_imm_bool(b, i), 1);
            else
               fork->path_ssa = nir_imm_bool(b, i);
            fork = fork->pathes[i].fork;
            break;
         }
      }
      assert(i < 2);
   }
}

static void
set_path_vars_cond(nir_builder *b, struct path_fork *fork, nir_src condition,
                   nir_block *then_block, nir_block *else_block)
{
   int i;
   while (fork) {
      for (i = 0; i < 2; i++) {
         if (_mesa_set_search(fork->pathes[i].reachable, then_block)) {
            if (_mesa_set_search(fork->pathes[i].reachable, else_block)) {
               if (fork->is_var)
                  nir_store_var(b, fork->path_var, nir_imm_bool(b, i), 1);
               else
                  fork->path_ssa = nir_imm_bool(b, i);
               fork = fork->pathes[i].fork;
               break;
            }
            else {
               assert(condition.is_ssa);
               nir_ssa_def *ssa_def = condition.ssa;
               assert(ssa_def->bit_size == 1);
               assert(ssa_def->num_components == 1);
               if (!i)
                  ssa_def = nir_inot(b, ssa_def);
               if (fork->is_var)
                  nir_store_var(b, fork->path_var, ssa_def, 1);
               else
                  fork->path_ssa = ssa_def;
               set_path_vars(b, fork->pathes[i].fork, then_block);
               set_path_vars(b, fork->pathes[!i].fork, else_block);
               return;
            }
         }
      }
      assert(i < 2);
   }
}

static void
route_to(nir_builder *b, struct routes *routing, nir_block *target)
{
   if (_mesa_set_search(routing->regular.reachable, target)) {
      set_path_vars(b, routing->regular.fork, target);
   }
   else if (_mesa_set_search(routing->brk.reachable, target)) {
      set_path_vars(b, routing->brk.fork, target);
      nir_jump(b, nir_jump_break);
   }
   else if (_mesa_set_search(routing->cont.reachable, target)) {
      set_path_vars(b, routing->cont.fork, target);
      nir_jump(b, nir_jump_continue);
   }
   else {
      assert(!target->successors[0]);   //target is endblock
      nir_jump(b, nir_jump_return);
   }
}

static void
route_to_cond(nir_builder *b, struct routes *routing, nir_src condition,
              nir_block *then_block, nir_block *else_block)
{
   if (_mesa_set_search(routing->regular.reachable, then_block)) {
      if (_mesa_set_search(routing->regular.reachable, else_block)) {
         set_path_vars_cond(b, routing->regular.fork, condition,
                            then_block, else_block);
         return;
      }
   } else if (_mesa_set_search(routing->brk.reachable, then_block)) {
      if (_mesa_set_search(routing->brk.reachable, else_block)) {
         set_path_vars_cond(b, routing->brk.fork, condition,
                            then_block, else_block);
         nir_jump(b, nir_jump_break);
         return;
      }
   } else if (_mesa_set_search(routing->cont.reachable, then_block)) {
      if (_mesa_set_search(routing->cont.reachable, else_block)) {
         set_path_vars_cond(b, routing->cont.fork, condition,
                            then_block, else_block);
         nir_jump(b, nir_jump_continue);
         return;
      }
   }

   //then and else blocks are in different routes
   nir_push_if_src(b, condition);
   route_to(b, routing, then_block);
   nir_push_else(b, NULL);
   route_to(b, routing, else_block);
   nir_pop_if(b, NULL);
}

static struct set *
fork_reachable(struct path_fork *fork)
{
   struct set *reachable = _mesa_set_clone(fork->pathes[0].reachable, fork);
   set_foreach(fork->pathes[1].reachable, entry)
      _mesa_set_add_pre_hashed(reachable, entry->hash, entry->key);
   return reachable;
}

static void
loop_routing_start(struct routes *routing, nir_builder *b,
                   struct path loop_path, struct set *reach)
{
   struct routes *routing_backup = ralloc(routing, struct routes);
   *routing_backup = *routing;
   int break_needed = 0;
   int continue_needed = 0;
   set_foreach(reach, entry) {
      if (_mesa_set_search(loop_path.reachable, entry->key))
         continue;
      if (_mesa_set_search(routing->regular.reachable, entry->key))
         continue;
      if (_mesa_set_search(routing->brk.reachable, entry->key)) {
         break_needed = 1;
         continue;
      }
      assert(_mesa_set_search(routing->cont.reachable, entry->key));
      continue_needed = 1;
   }
   routing->brk = routing_backup->regular;
   routing->cont = loop_path;
   routing->regular = loop_path;
   routing->loop_backup = routing_backup;
   if (break_needed) {
      struct path_fork *fork = ralloc(routing_backup, struct path_fork);
      fork->is_var = true;
      fork->path_var = nir_local_variable_create(b->impl, glsl_bool_type(),
                                                 "path_break");
      fork->pathes[0] = routing->brk;
      fork->pathes[1] = routing_backup->brk;
      routing->brk.fork = fork;
      routing->brk.reachable = fork_reachable(fork);
   }
   if (continue_needed) {
      struct path_fork *fork = ralloc(routing_backup, struct path_fork);
      fork->is_var = true;
      fork->path_var = nir_local_variable_create(b->impl, glsl_bool_type(),
                                                 "path_continue");
      fork->pathes[0] = routing->brk;
      fork->pathes[1] = routing_backup->cont;
      routing->brk.fork = fork;
      routing->brk.reachable = fork_reachable(fork);
   }
   nir_push_loop(b);
}

static nir_ssa_def *
fork_condition(nir_builder *b, struct path_fork *fork)
{
   nir_ssa_def *ret;
   if (fork->is_var) {
      ret = nir_load_var(b, fork->path_var);
   }
   else
      ret = fork->path_ssa;
   return ret;
}

static void
loop_routing_end(struct routes *routing, nir_builder *b)
{
   struct routes *routing_backup = routing->loop_backup;
   assert(routing->cont.fork == routing->regular.fork);
   assert(routing->cont.reachable == routing->regular.reachable);
   nir_pop_loop(b, NULL);
   if (routing->brk.fork && routing->brk.fork->pathes[1].reachable ==
       routing_backup->cont.reachable) {
      assert(!(routing->brk.fork->is_var &&
               strcmp(routing->brk.fork->path_var->name, "path_continue")));
      nir_push_if_src(b, nir_src_for_ssa(
                         fork_condition(b, routing->brk.fork)));
      nir_jump(b, nir_jump_continue);
      nir_pop_if(b, NULL);
      routing->brk = routing->brk.fork->pathes[0];
   }
   if (routing->brk.fork && routing->brk.fork->pathes[1].reachable ==
       routing_backup->brk.reachable) {
      assert(!(routing->brk.fork->is_var &&
               strcmp(routing->brk.fork->path_var->name, "path_break")));
      nir_push_if_src(b, nir_src_for_ssa(
                         fork_condition(b, routing->brk.fork)));
      nir_jump(b, nir_jump_break);
      nir_pop_if(b, NULL);
      routing->brk = routing->brk.fork->pathes[0];
   }
   assert(routing->brk.fork == routing_backup->regular.fork);
   assert(routing->brk.reachable == routing_backup->regular.reachable);
   *routing = *routing_backup;
   ralloc_free(routing_backup);
}

static int
set_common(struct set *set1, struct set *set2)
{
   set_foreach(set1, entry) {
      if (_mesa_set_search_pre_hashed(set2, entry->hash, entry->key))
         return 1;
   }
   return 0;
}

/**
 * generates a list of all blocks dominated by the loop header, but the
 * control flow can't go back to the loop header from the block
 * also generates a list of all blocks that can be reached from within the
 * loop
 * \param  loop_heads  set of loop heads. All blocks inside the loop will be
 *                     added to this set
 * \param  outside  all blocks directly outside the loop will be added
 * \param  reach  all blocks reachable from the loop will be added
 */
static void
inside_outside(nir_block *block, struct set *loop_heads, struct set *outside,
               struct set *reach, struct set *brk_reachable)
{
   assert(_mesa_set_search(loop_heads, block));
   struct set *remaining = _mesa_pointer_set_create(NULL);
   for (int i = 0; i < block->num_dom_children; i++) {
      if (!_mesa_set_search(brk_reachable, block->dom_children[i]))
         _mesa_set_add(remaining, block->dom_children[i]);
   }
   bool progress = true;
   while (remaining->entries && progress) {
      progress = false;
      set_foreach(remaining, child_entry) {
         nir_block *dom_child = (nir_block *) child_entry->key;
         bool can_jump_back = false;
         set_foreach(dom_child->dom_frontier, entry) {
            if (entry->key == dom_child)
               continue;
            if (_mesa_set_search_pre_hashed(remaining, entry->hash,
                                            entry->key)) {
               can_jump_back = true;
               break;
            }
            if (_mesa_set_search_pre_hashed(loop_heads, entry->hash,
                                            entry->key)) {
               can_jump_back = true;
               break;
            }
         }
         if (!can_jump_back) {
            _mesa_set_add_pre_hashed(outside, child_entry->hash,
                                     child_entry->key);
            _mesa_set_remove(remaining, child_entry);
            progress = true;
         }
      }
   }
   set_foreach(remaining, entry) {
      _mesa_set_add_pre_hashed(loop_heads, entry->hash, entry->key);
   }
   set_foreach(remaining, entry) {
      inside_outside((nir_block *) entry->key, loop_heads, outside, reach,
                     brk_reachable);
   }
   _mesa_set_destroy(remaining, NULL);
   remaining = NULL;
   if (block->successors[0] && block->successors[0]->successors[0] &&
       !_mesa_set_search(loop_heads, block->successors[0])) {
      _mesa_set_add(reach, block->successors[0]);
   }
   if (block->successors[1] && block->successors[1]->successors[0] &&
       !_mesa_set_search(loop_heads, block->successors[1])) {
      _mesa_set_add(reach, block->successors[1]);
   }
}

static struct path_fork *
select_fork(struct set *reachable, nir_function_impl *impl, bool need_var)
{
   struct path_fork *fork = NULL;
   if (reachable->entries > 1) {
      fork = ralloc(reachable, struct path_fork);
      fork->is_var = need_var;
      if (need_var)
         fork->path_var = nir_local_variable_create(impl, glsl_bool_type(),
                                                    "path_select");
      fork->pathes[0].reachable = _mesa_pointer_set_create(fork);
      struct set_entry *entry = NULL;
      while (fork->pathes[0].reachable->entries < reachable->entries / 2 &&
             (entry = _mesa_set_next_entry(reachable, entry))) {
         _mesa_set_add_pre_hashed(fork->pathes[0].reachable,
                                  entry->hash, entry->key);
      }
      fork->pathes[0].fork = select_fork(fork->pathes[0].reachable, impl,
                                         need_var);
      fork->pathes[1].reachable = _mesa_pointer_set_create(fork);
      while ((entry = _mesa_set_next_entry(reachable, entry))) {
         _mesa_set_add_pre_hashed(fork->pathes[1].reachable,
                                  entry->hash, entry->key);
      }
      fork->pathes[1].fork = select_fork(fork->pathes[1].reachable, impl,
                                         need_var);
   }
   return fork;
}

/**
 * organize a set of blocks into a list list of levels with one or more
 * blocks, so that every block is before all blocks it can reach.
 * Also creates all path variables needed, for the control flow between the
 * block
 */
static void
organize_levels(struct exec_list *levels, struct set *remaining,
                struct set *reach, struct routes *routing,
                nir_function_impl *impl, bool is_domminated)
{
   void *mem_ctx = ralloc_parent(remaining);
   //blocks that can be reached by the remaining blocks
   struct set *remaining_frontier = _mesa_pointer_set_create(mem_ctx);
   //targets of active skip path
   struct set *skip_targets = _mesa_pointer_set_create(mem_ctx);
   exec_list_make_empty(levels);
   while (remaining->entries) {
      _mesa_set_clear(remaining_frontier, NULL);
      set_foreach(remaining, entry) {
         nir_block *remain_block = (nir_block *) entry->key;
         set_foreach(remain_block->dom_frontier, frontier_entry) {
            nir_block *frontier = (nir_block *) frontier_entry->key;
            if (frontier != remain_block) {
               _mesa_set_add(remaining_frontier, frontier);
            }
         }
      }
      struct strct_lvl *curr_level = ralloc(mem_ctx, struct strct_lvl);
      curr_level->blocks = _mesa_pointer_set_create(curr_level);
      set_foreach(remaining, entry) {
         nir_block *candidate = (nir_block *) entry->key;
         if (!_mesa_set_search(remaining_frontier, candidate)) {
            _mesa_set_add(curr_level->blocks, candidate);
            _mesa_set_remove_key(remaining, candidate);
         }
      }
      curr_level->irreducible = !curr_level->blocks->entries;
      if (curr_level->irreducible) {
         nir_block *candidate = (nir_block *)
            _mesa_set_next_entry(remaining, NULL)->key;
         nir_block *to_be_added;
         struct set *old_candidates = _mesa_pointer_set_create(curr_level);
         while (candidate) {
            _mesa_set_add(old_candidates, candidate);
            to_be_added = candidate;
            candidate = NULL;
            _mesa_set_clear(curr_level->blocks, NULL);
            while (to_be_added) {
               _mesa_set_add(curr_level->blocks, to_be_added);
               to_be_added = NULL;
               set_foreach(remaining, entry) {
                  nir_block *remaining_block = (nir_block *) entry->key;
                  if (!_mesa_set_search(curr_level->blocks, remaining_block)
                      && set_common(remaining_block->dom_frontier,
                                    curr_level->blocks)) {
                     if (_mesa_set_search(old_candidates, remaining_block))
                        to_be_added = remaining_block;
                     else
                        candidate = remaining_block;
                     break;
                  }
               }
            }
         }
         _mesa_set_destroy(old_candidates, NULL);
         old_candidates = NULL;
         struct set *loop_heads = _mesa_set_clone(curr_level->blocks,
                                                  curr_level);
         curr_level->reach = _mesa_pointer_set_create(curr_level);
         set_foreach(curr_level->blocks, entry) {
            _mesa_set_remove_key(remaining, entry->key);
            inside_outside((nir_block *) entry->key, loop_heads, remaining,
                           curr_level->reach, routing->brk.reachable);
         }
         _mesa_set_destroy(loop_heads, NULL);
      }
      assert(curr_level->blocks->entries);
      curr_level->skip_start = 0;
      struct strct_lvl *prev_level = NULL;
      struct exec_node *tail;
      if ((tail = exec_list_get_tail(levels)))
         prev_level = exec_node_data(struct strct_lvl, tail, node);
      if (skip_targets->entries) {
         set_foreach(skip_targets, entry) {
            if (_mesa_set_search_pre_hashed(curr_level->blocks,
                                            entry->hash, entry->key)) {
               _mesa_set_remove(skip_targets, entry);
               prev_level->skip_end = 1;
               curr_level->skip_start = !!skip_targets->entries;
            }
         }
      }
      struct set *prev_frontier = NULL;
      if (!prev_level) {
         prev_frontier = reach;
      } else if (prev_level->irreducible) {
         prev_frontier = prev_level->reach;
      } else {
         set_foreach(curr_level->blocks, blocks_entry) {
            nir_block *level_block = (nir_block *) blocks_entry->key;
            if (!prev_frontier) {
               prev_frontier = curr_level->blocks->entries == 1 ?
                  level_block->dom_frontier :
                  _mesa_set_clone(level_block->dom_frontier, prev_level);
            } else {
               set_foreach(level_block->dom_frontier, entry)
                  _mesa_set_add_pre_hashed(prev_frontier, entry->hash,
                                           entry->key);
            }
         }
      }
      bool is_in_skip = !!skip_targets->entries;
      set_foreach(prev_frontier, entry) {
         if (_mesa_set_search(remaining, entry->key) ||
             (_mesa_set_search(routing->regular.reachable, entry->key) &&
              !_mesa_set_search(routing->brk.reachable, entry->key) &&
              !_mesa_set_search(routing->cont.reachable, entry->key))) {
            _mesa_set_add_pre_hashed(skip_targets, entry->hash, entry->key);
            if (is_in_skip)
               prev_level->skip_end = 1;
            curr_level->skip_start = 1;
         }
      }
      curr_level->skip_end = 0;
      exec_list_push_tail(levels, &curr_level->node);
   }
   if (skip_targets->entries)
      exec_node_data(struct strct_lvl, exec_list_get_tail(levels), node)
      ->skip_end = 1;
   _mesa_set_destroy(remaining_frontier, NULL);
   remaining_frontier = NULL;
   _mesa_set_destroy(skip_targets, NULL);
   skip_targets = NULL;

   //iterate throught all levels reverse and create all the pathes and forks
   struct path path_after_skip;

   foreach_list_typed_reverse(struct strct_lvl, level, node, levels) {
      bool need_var = !(is_domminated && exec_node_get_prev(&level->node)
                                         == &levels->head_sentinel);
      level->out_path = routing->regular;
      if (level->skip_end) {
         path_after_skip = routing->regular;
      }
      routing->regular.reachable = level->blocks;
      routing->regular.fork = select_fork(routing->regular.reachable, impl,
                                          need_var);
      if (level->skip_start) {
         struct path_fork *fork = ralloc(level, struct path_fork);
         fork->is_var = need_var;
         if (need_var)
            fork->path_var = nir_local_variable_create(impl, glsl_bool_type(),
                                                       "path_conditional");
         fork->pathes[0] = path_after_skip;
         fork->pathes[1] = routing->regular;
         routing->regular.fork = fork;
         routing->regular.reachable = fork_reachable(fork);
      }
   }
}

static void
nir_structurize(struct routes *routing, nir_builder *b, nir_block *block);

static void
select_blocks(struct routes *routing, nir_builder *b, struct path in_path) {
   if (!in_path.fork) {
      nir_structurize(routing, b, (nir_block *)
                      _mesa_set_next_entry(in_path.reachable, NULL)->key);
   } else {
      assert(!(in_path.fork->is_var &&
               strcmp(in_path.fork->path_var->name, "path_select")));
      nir_push_if_src(b, nir_src_for_ssa(fork_condition(b, in_path.fork)));
      select_blocks(routing, b, in_path.fork->pathes[1]);
      nir_push_else(b, NULL);
      select_blocks(routing, b, in_path.fork->pathes[0]);
      nir_pop_if(b, NULL);
   }
}

static void
plant_levels(struct exec_list *levels, struct routes *routing,
             nir_builder *b)
{
   //place all dominated blocks and build the path forks
   struct exec_node *list_node;
   while ((list_node = exec_list_pop_head(levels))) {
      struct strct_lvl *curr_level =
         exec_node_data(struct strct_lvl, list_node, node);
      if (curr_level->skip_start) {
         assert(routing->regular.fork);
         assert(!(routing->regular.fork->is_var && strcmp(
             routing->regular.fork->path_var->name, "path_conditional")));
         nir_push_if_src(b, nir_src_for_ssa(
                            fork_condition(b, routing->regular.fork)));
         routing->regular = routing->regular.fork->pathes[1];
      }
      struct path in_path = routing->regular;
      routing->regular = curr_level->out_path;
      if (curr_level->irreducible)
         loop_routing_start(routing, b, in_path, curr_level->reach);
      select_blocks(routing, b, in_path);
      if (curr_level->irreducible)
         loop_routing_end(routing, b);
      if (curr_level->skip_end) {
         nir_pop_if(b, NULL);
      }
      ralloc_free(curr_level);
   }
}

/**
 * builds the control flow of a block and all its dominance children
 */
static void
nir_structurize(struct routes *routing, nir_builder *b, nir_block *block)
{
   void *mem_ctx = ralloc_context(routing);
   struct exec_list levels;
   struct exec_list outside_levels;
   struct set *reach;
   struct set *remaining = _mesa_pointer_set_create(mem_ctx);
   for (int i = 0; i < block->num_dom_children; i++) {
      if (!_mesa_set_search(routing->brk.reachable, block->dom_children[i]))
         _mesa_set_add(remaining, block->dom_children[i]);
   }
   int is_looped = _mesa_set_search(block->dom_frontier, block) != NULL;
   if (is_looped) {
      struct set *loop_heads = _mesa_pointer_set_create(mem_ctx);
      _mesa_set_add(loop_heads, block);
      struct set *outside = _mesa_pointer_set_create(mem_ctx);
      reach = _mesa_pointer_set_create(mem_ctx);
      inside_outside(block, loop_heads, outside, reach,
                     routing->brk.reachable);
      _mesa_set_destroy(loop_heads, NULL);
      loop_heads = NULL;
      set_foreach(outside, entry)
         _mesa_set_remove_key(remaining, entry->key);
      organize_levels(&outside_levels, outside, reach, routing, b->impl,
                      false);
      _mesa_set_destroy(outside, NULL);
      outside = NULL;
      struct path loop_path;
      loop_path.reachable = _mesa_pointer_set_create(mem_ctx);
      _mesa_set_add(loop_path.reachable, block);
      loop_path.fork = NULL;
      loop_routing_start(routing, b, loop_path, reach);
      _mesa_set_destroy(reach, NULL);
      reach = NULL;
   }
   reach = _mesa_pointer_set_create(mem_ctx);
   if (block->successors[0]->successors[0]) {   //it is not the end_block
      _mesa_set_add(reach, block->successors[0]);
   }
   if (block->successors[1] && block->successors[1]->successors[0]) {
      _mesa_set_add(reach, block->successors[1]);
   }
   organize_levels(&levels, remaining, reach, routing, b->impl, true);
   _mesa_set_destroy(remaining, NULL);
   remaining = NULL;
   _mesa_set_destroy(reach, NULL);
   reach = NULL;

   //push all instructions of this block, without the jump instr
   nir_jump_instr *jump_instr = NULL;
   nir_foreach_instr_safe(instr, block) {
      if (instr->type == nir_instr_type_jump) {
         jump_instr = nir_instr_as_jump(instr);
         break;
      }
      nir_instr_remove_v(instr);
      nir_builder_instr_insert(b, instr);
   }

   //find path to the successor blocks
   if (block->successors[1]) {  //two way branching
      assert(jump_instr);
      assert(jump_instr->type == nir_jump_goto_if);
      if (block->successors[0] == jump_instr->target) {
         route_to_cond(b, routing, jump_instr->condition,
                       block->successors[0], block->successors[1]);
      }
      else {
         route_to_cond(b, routing, jump_instr->condition,
                       block->successors[1], block->successors[0]);
      }
      list_del(&jump_instr->condition.use_link);
   }
   else {                       //only one successor
      route_to(b, routing, block->successors[0]);
   }

   plant_levels(&levels, routing, b);
   if (is_looped) {
      loop_routing_end(routing, b);
      plant_levels(&outside_levels, routing, b);
   }
   ralloc_free(mem_ctx);
}

static void
nir_lower_goto_ifs_impl(nir_function_impl *impl)
{
   nir_block *start_block_new;
   nir_builder b;
   nir_cf_list cf_list;

   nir_metadata_require(impl, nir_metadata_dominance);

   nir_builder_init(&b, impl);
   start_block_new = nir_block_create(b.shader);
   start_block_new->cf_node.parent = &impl->cf_node;
   cf_list.impl = impl;
   cf_list.list = impl->body;
   exec_list_make_empty(&impl->body);
   start_block_new->successors[0] = impl->end_block;
   _mesa_set_add(impl->end_block->predecessors, start_block_new);
   exec_list_push_tail(&impl->body, &start_block_new->cf_node.node);
   nir_metadata_preserve(impl, nir_metadata_none);
   b.cursor = nir_after_block(start_block_new);
   struct routes *routing = ralloc(b.shader, struct routes);
   routing->regular.reachable = _mesa_pointer_set_create(routing);
   _mesa_set_add(routing->regular.reachable, impl->end_block);
   struct set *empty = _mesa_pointer_set_create(routing);
   routing->regular.fork = NULL;
   routing->brk.reachable = empty;
   routing->brk.fork = NULL;
   routing->cont.reachable = empty;
   routing->cont.fork = NULL;
   nir_structurize(routing, &b,
                   nir_cf_node_as_block(exec_node_data
                                        (nir_cf_node,
                                         exec_list_get_head(&cf_list.list),
                                         node)));
   assert(routing->regular.fork == NULL);
   assert(routing->brk.fork == NULL);
   assert(routing->cont.fork == NULL);
   assert(routing->brk.reachable == empty);
   assert(routing->cont.reachable == empty);
   _mesa_set_destroy(routing->regular.reachable, NULL);
   _mesa_set_destroy(empty, NULL);
   ralloc_free(routing);
   nir_cf_delete(&cf_list);

   return;
}

bool
nir_lower_goto_ifs(nir_shader *shader)
{
   if (shader->structured)
      return false;

   nir_foreach_function(function, shader) {
      if (function->impl)
         nir_lower_goto_ifs_impl(function->impl);
   }

   shader->structured = true;

   return true;
}
