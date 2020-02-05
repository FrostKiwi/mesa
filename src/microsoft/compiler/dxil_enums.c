/*
 * Copyright (c) 2020 Collabora Ltd.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * on the rights to use, copy, modify, merge, publish, distribute, sub
 * license, and/or sell copies of the Software, and to permit persons to whom
 * the Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHOR(S) AND/OR THEIR SUPPLIERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
 * USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "dxil_enums.h"

#include "nir.h"
#include "nir_types.h"

#include "util/u_debug.h"

enum dxil_prog_sig_comp_type dxil_get_prog_sig_comp_type(const struct glsl_type *type)
{
   switch (glsl_get_base_type(type)) {
   case GLSL_TYPE_UINT: return DXIL_PROG_SIG_COMP_TYPE_UINT32;
   case GLSL_TYPE_INT: return DXIL_PROG_SIG_COMP_TYPE_SINT32;
   case GLSL_TYPE_FLOAT: return DXIL_PROG_SIG_COMP_TYPE_FLOAT32;
   case GLSL_TYPE_FLOAT16: return DXIL_PROG_SIG_COMP_TYPE_FLOAT16;
   case GLSL_TYPE_DOUBLE: return DXIL_PROG_SIG_COMP_TYPE_FLOAT64;
   case GLSL_TYPE_UINT16: return DXIL_PROG_SIG_COMP_TYPE_UINT16;
   case GLSL_TYPE_INT16: return DXIL_PROG_SIG_COMP_TYPE_SINT16;
   case GLSL_TYPE_UINT64: return DXIL_PROG_SIG_COMP_TYPE_UINT64;
   case GLSL_TYPE_INT64: return DXIL_PROG_SIG_COMP_TYPE_SINT64;
   case GLSL_TYPE_BOOL: return DXIL_PROG_SIG_COMP_TYPE_UINT32;
   default:
      debug_printf("unexpected type: %s\n", glsl_get_type_name(type));
      return DXIL_PROG_SIG_COMP_TYPE_UNKNOWN;
   }
}

enum dxil_component_type dxil_get_comp_type(const struct glsl_type *type)
{
   switch (glsl_get_base_type(type)) {
   case GLSL_TYPE_UINT: return DXIL_COMP_TYPE_U32;
   case GLSL_TYPE_INT: return DXIL_COMP_TYPE_I32;
   case GLSL_TYPE_FLOAT: return DXIL_COMP_TYPE_F32;
   case GLSL_TYPE_FLOAT16: return DXIL_COMP_TYPE_F16;
   case GLSL_TYPE_DOUBLE: return DXIL_COMP_TYPE_F64;
   case GLSL_TYPE_UINT16: return DXIL_COMP_TYPE_U16;
   case GLSL_TYPE_INT16: return DXIL_COMP_TYPE_I16;
   case GLSL_TYPE_UINT64: return DXIL_COMP_TYPE_U64;
   case GLSL_TYPE_INT64: return DXIL_COMP_TYPE_I64;
   case GLSL_TYPE_BOOL: return DXIL_COMP_TYPE_I1;

   default:
      debug_printf("type: %s\n", glsl_get_type_name(type));
      unreachable("unexpected glsl type");
   }
}
