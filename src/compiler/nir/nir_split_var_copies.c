/*
 * Copyright © 2014 Intel Corporation
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 *
 * Authors:
 *    Jason Ekstrand (jason@jlekstrand.net)
 *
 */

#include "nir.h"
#include "nir_builder.h"
#include "nir_deref.h"

/*
 * Implements "copy splitting" which is similar to structure splitting only
 * it works on copy operations rather than the datatypes themselves.  The
 * GLSL language allows you to copy one variable to another an entire
 * structure (which may contain arrays or other structures) at a time.
 * Normally, in a language such as C this would be handled by a "structure
 * splitting" pass that breaks up the structures.  Unfortunately for us,
 * structures used in inputs or outputs can't be split.  Therefore,
 * regardlesss of what we do, we have to be able to copy to/from
 * structures.
 *
 * The primary purpose of structure splitting is to allow you to better
 * optimize variable access and lower things to registers where you can.
 * The primary issue here is that, if you lower the copy to a bunch of
 * loads and stores, you loose a lot of information about the copy
 * operation that you would like to keep around.  To solve this problem, we
 * have a "copy splitting" pass that, instead of splitting the structures
 * or lowering the copy into loads and storres, splits the copy operation
 * into a bunch of copy operations one for each leaf of the structure tree.
 * If an intermediate array is encountered, it is referenced with a
 * wildcard reference to indicate that the entire array is to be copied.
 *
 * As things become direct, array copies may be able to be losslessly
 * lowered to having fewer and fewer wildcards.  However, until that
 * happens we want to keep the information about the arrays intact.
 *
 * Prior to the copy splitting pass, there are no wildcard references but
 * there may be incomplete references where the tail of the deref chain is
 * an array or a structure and not a specific element.  After the copy
 * splitting pass has completed, every variable deref will be a full-length
 * dereference pointing to a single leaf in the structure type tree with
 * possibly a few wildcard array dereferences.
 */

static void
get_natural_align_mul_offset(nir_deref_instr *deref,
                             unsigned *align_mul, unsigned *align_offset,
                             glsl_type_size_align_func size_align)
{
   unsigned size, align;
   nir_deref_path path;
   nir_deref_path_init(&path, deref, NULL);

   size_align(path.path[0]->type, &size, align_mul);
   *align_offset = 0;

   for (nir_deref_instr **p = &path.path[1]; *p; p++) {
      if ((*p)->deref_type == nir_deref_type_array) {

         size_align((*p)->type, &size, &align);
         size = ALIGN_POT(size, align);

         if (nir_src_is_const((*p)->arr.index)) {
            *align_offset += nir_src_as_uint((*p)->arr.index) * size;
         } else {
            /* If we can't guess the index we have to assume the worst
             * alignment, which depends on the base align mul, the current
             * offset, and the element size. Offset is reset to 0.
             */
            unsigned elem_align_mul = 1 << (ffs(size) - 1);
            unsigned base_align_mul =
               *align_offset ? 1 << (ffs(*align_offset) - 1) : *align_mul;

            *align_mul = MIN3(*align_mul, elem_align_mul, base_align_mul);
            *align_offset = 0;
         }
      } else if ((*p)->deref_type == nir_deref_type_struct) {
         /* p starts at path[1], so this is safe */
         nir_deref_instr *parent = *(p - 1);
         *align_offset += glsl_get_struct_field_offset(parent->type,
                                                       (*p)->strct.index);
      } else {
         unreachable("Unsupported deref type");
      }
   }

   nir_deref_path_finish(&path);
}

static void
split_deref_copy_instr(nir_builder *b,
                       nir_deref_instr *dst, nir_deref_instr *src,
                       enum gl_access_qualifier dst_access,
                       enum gl_access_qualifier src_access,
                       unsigned dst_align_mul, unsigned dst_align_offset,
                       unsigned src_align_mul, unsigned src_align_offset,
                       glsl_type_size_align_func size_align)
{
   assert(glsl_get_bare_type(dst->type) ==
          glsl_get_bare_type(src->type));
   if (glsl_type_is_vector_or_scalar(src->type)) {
      dst_align_offset = dst_align_mul ? dst_align_offset % dst_align_mul : 0;
      src_align_offset = src_align_mul ? src_align_offset % src_align_mul : 0;
      nir_copy_deref_with_access_and_align(b, dst, src, dst_access, src_access,
                                           dst_align_mul, dst_align_offset,
                                           src_align_mul, src_align_offset);
   } else if (glsl_type_is_struct_or_ifc(src->type)) {
      for (unsigned i = 0; i < glsl_get_length(src->type); i++) {
         unsigned offset = glsl_get_struct_field_offset(src->type, i);

         dst_align_offset += offset;
         src_align_offset += offset;
         split_deref_copy_instr(b, nir_build_deref_struct(b, dst, i),
                                   nir_build_deref_struct(b, src, i),
                                   dst_access, src_access,
                                   dst_align_mul, dst_align_offset,
                                   src_align_mul, src_align_offset,
                                   size_align);
      }
   } else {
      assert(glsl_type_is_matrix(src->type) || glsl_type_is_array(src->type));
      const struct glsl_type *elem_type = glsl_get_array_element(src->type);
      unsigned elem_size, elem_align;

      size_align(elem_type, &elem_size, &elem_align);
      elem_size = ALIGN_POT(elem_size, elem_align);

      /* Reset the offset when crossing an array, and adjust the mul
       * accordingly.
       */
      unsigned elem_align_mul = 1 << (ffs(elem_size) - 1);
      unsigned src_array_align_mul =
         src_align_offset ? 1 << (ffs(src_align_offset) - 1) : src_align_mul;
      unsigned dst_array_align_mul =
         dst_align_offset ? 1 << (ffs(dst_align_offset) - 1) : dst_align_mul;

      dst_align_mul = MIN3(dst_align_mul, elem_align_mul, dst_array_align_mul);
      src_align_mul = MIN3(src_align_mul, elem_align_mul, src_array_align_mul);
      dst_align_offset = 0;
      src_align_offset = 0;
      split_deref_copy_instr(b, nir_build_deref_array_wildcard(b, dst),
                                nir_build_deref_array_wildcard(b, src),
                                dst_access, src_access,
                                dst_align_mul, dst_align_offset,
                                src_align_mul, src_align_offset,
                                size_align);
   }
}

static bool
split_var_copies_impl(nir_function_impl *impl,
                      glsl_type_size_align_func size_align)
{
   bool progress = false;

   nir_builder b;
   nir_builder_init(&b, impl);

   nir_foreach_block(block, impl) {
      nir_foreach_instr_safe(instr, block) {
         if (instr->type != nir_instr_type_intrinsic)
            continue;

         nir_intrinsic_instr *copy = nir_instr_as_intrinsic(instr);
         if (copy->intrinsic != nir_intrinsic_copy_deref)
            continue;

         b.cursor = nir_instr_remove(&copy->instr);

         nir_deref_instr *dst =
            nir_instr_as_deref(copy->src[0].ssa->parent_instr);
         nir_deref_instr *src =
            nir_instr_as_deref(copy->src[1].ssa->parent_instr);

         unsigned dst_align_mul = nir_intrinsic_dst_align_mul(copy);
         unsigned dst_align_offset = nir_intrinsic_dst_align_offset(copy);
         unsigned src_align_mul = nir_intrinsic_src_align_mul(copy);
         unsigned src_align_offset = nir_intrinsic_src_align_offset(copy);

         if (!dst_align_mul)
            get_natural_align_mul_offset(src, &dst_align_mul,
                                         &dst_align_offset, size_align);

         if (!src_align_mul)
            get_natural_align_mul_offset(src, &src_align_mul,
                                         &src_align_offset, size_align);

         split_deref_copy_instr(&b, dst, src,
                                nir_intrinsic_dst_access(copy),
                                nir_intrinsic_src_access(copy),
                                dst_align_mul, dst_align_offset,
                                src_align_mul, src_align_offset,
                                size_align);

         progress = true;
      }
   }

   if (progress) {
      nir_metadata_preserve(impl, nir_metadata_block_index |
                                  nir_metadata_dominance);
   } else {
#ifndef NDEBUG
      impl->valid_metadata &= ~nir_metadata_not_properly_reset;
#endif
   }

   return progress;
}

bool
nir_split_var_copies(nir_shader *shader)
{
   glsl_type_size_align_func size_align =
      shader->info.stage == MESA_SHADER_KERNEL ?
      glsl_get_cl_type_size_align :
      glsl_get_natural_size_align_bytes;

   bool progress = false;

   nir_foreach_function(function, shader) {
      if (function->impl)
         progress =
            split_var_copies_impl(function->impl, size_align) || progress;
   }

   return progress;
}
