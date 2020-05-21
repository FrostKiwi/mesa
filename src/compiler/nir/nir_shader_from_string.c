/*
 * Copyright © 2019 Collabora LTD
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
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

#include "nir.h"
#include "nir_builder.h"

#ifdef WIN32
#define strtok_r strtok_s
#endif

#define error_message(format, ...) \
   fprintf(stderr, format, __VA_ARGS__)

typedef struct {
   struct exec_node node;
   nir_phi_instr  *phi;
   char *source_list_string;
} phi_handle;

typedef struct {
   nir_builder *b;
   void *memctx;
   int nesting_level;
   struct exec_list cf_nodes;
   struct exec_list phi_handles;
} block_tracker;

static char *next_line(char *s)
{
   s = strchr(s, '\n');
   while (s && strchr(" \n\t\r", *s))
      ++s;
   return s;
}

static
nir_variable_mode get_variable_mode(const char *string)
{
   if (!strcmp(string, "shader_in"))
      return nir_var_shader_in;

   if (!strcmp(string, "shader_out"))
      return nir_var_shader_out;

   if (!strcmp(string, "uniform"))
      return nir_var_uniform;

   if (!strcmp(string, "ubo"))
      return nir_var_mem_ubo;

   if (!strcmp(string, "function_temp"))
      return nir_var_function_temp;

   if (!strcmp(string, "system"))
      return nir_var_system_value;

   error_message("Unsupported variable mode '%s'\n", string);

   return (nir_variable_mode)0;
}

static
int get_vert_attr(const char *slot)
{
   for (unsigned i = 0; i < VERT_ATTRIB_MAX; ++i) {
      const char *attr = gl_vert_attrib_name(i);
      if (!strncmp(slot, attr, strlen(attr)))
          return (int)i;
   }
   return -1;
}

static
int get_frag_out(const char *slot)
{
   for (unsigned i = 0; i <= FRAG_RESULT_DATA7 ; ++i) {
      const char *attr = gl_frag_result_name(i);
      if (!strncmp(slot, attr, strlen(attr)))
          return (int)i;
   }
   return -1;
}

static
int get_varying(const char *slot)
{
   for (unsigned i = 0; i < VARYING_SLOT_MAX; ++i) {
      const char *attr = gl_varying_slot_name(i);
      if (!strncmp(slot, attr, strlen(attr)))
          return (int)i;
   }
   return -1;
}

static bool split_index(char *in_namebuf, char *out_namebuf,
                        int *index, char *indirect_index)
{

   assert(in_namebuf);
   assert(out_namebuf);
   assert(index);

   char *saveptr;

   char *end_name = strtok_r(in_namebuf, ")[", &saveptr);

   if (sscanf(end_name, "%s", out_namebuf) != 1) {
      error_message("Unable tp split name from %s\n", end_name);
      return false;
   }

   char *end_count = strtok_r(NULL, "]", &saveptr);

   if (end_count) {
      if (*end_count == '[') ++end_count;
      if (sscanf(end_count, "%d", index) != 1) {
         if (indirect_index)  {
            *index = -1;
            if (sscanf(end_count, "%s", indirect_index) != 1) {
               error_message("Unable tp read indirect index from %s\n", end_count);
               return false;
            }
         } else {
            error_message("Unable tp read index from %s\n", end_count);
            return false;
         }
      }
   } else {
      *index = 0;
   }
   return true;
}

static
const struct glsl_type *get_sampler_type(char *stype)
{
   enum glsl_sampler_dim dim;

   if (stype[0] == '1') {
      dim = GLSL_SAMPLER_DIM_1D;
      stype += 2;
   } else if (stype[0] == '2') {
      dim = GLSL_SAMPLER_DIM_2D;
      stype += 2;
   } else if (stype[0] == '3') {
      dim = GLSL_SAMPLER_DIM_3D;
      stype += 2;
   } else if (!*stype) {
      return glsl_bare_sampler_type();
   } else {
      error_message("TODO: support sampler with dim %s\n", stype);
      return NULL;
   }

   if (!*stype)
      return glsl_sampler_type(dim, false, false, GLSL_TYPE_FLOAT);

   if (!strcmp(stype, "Shadow"))
      return glsl_sampler_type(dim, true, false, GLSL_TYPE_FLOAT);
   if (!strcmp(stype, "Array"))
      return glsl_sampler_type(dim, false, true, GLSL_TYPE_FLOAT);

   error_message("TODO: support sampler %s\n", stype);
   return NULL;
}

static
const struct glsl_type *get_vector_type(char *dim_str)
{
   int dim = 0;
   if (sscanf(dim_str, "%d", &dim) != 1)
      return NULL;
   return  glsl_vec_type(dim);
}

static
const struct glsl_type *get_matrix_type(char *dim_str)
{
   int nx, ny;

   char *x = strchr(dim_str, 'x');
   if (x)  {
      *x = ' ';
      if (sscanf(dim_str, "%d %d", &nx, &ny) != 2)
         return NULL;
   } else {
         if (sscanf(dim_str, "%d", &nx) != 1)
            return NULL;
         ny = nx;
   }
   return glsl_matrix_type(GLSL_TYPE_FLOAT, nx, ny);
}


static
const struct glsl_type *get_variable_base_type(char *stype)
{

   if (!strncmp(stype, "sampler", 7))
      return get_sampler_type(stype + 7);

   if (!strcmp(stype, "bool"))
      return glsl_bool_type();
   if (!strcmp(stype, "int"))
      return glsl_int_type();
   if (!strcmp(stype, "uint"))
      return glsl_uint_type();
   if (!strcmp(stype, "float"))
      return glsl_float_type();
   if (!strncmp(stype, "vec", 3))
      return get_vector_type(stype + 3);
   if (!strncmp(stype, "mat", 3))
      return get_matrix_type(stype + 3);

   error_message("Unsupported type '%s'\n", stype);

   return NULL;
}

static
const struct glsl_type *get_variable_type(char *type_string)
{

   char stype[64];
   int size = -1;


   if (!split_index(type_string, stype, &size, NULL)) {
      error_message("Error splitting '%s'\n", type_string);
      return NULL;
   }

   const struct glsl_type *base_type = get_variable_base_type(stype);
   if (!base_type)
      return NULL;

   if (size > 0)
      return glsl_array_type(base_type, size, 0);
   return base_type;
}

static
enum glsl_interp_mode get_interp(const char *string)
{
   if (!strcmp(string, "INTERP_MODE_SMOOTH"))
      return INTERP_MODE_SMOOTH;
   if (!strcmp(string, "INTERP_MODE_FLAT"))
      return INTERP_MODE_FLAT;
   if (!strcmp(string, "INTERP_MODE_NOPERSPECTIVE"))
      return INTERP_MODE_NOPERSPECTIVE;
   return INTERP_MODE_NONE;
}


static
int get_array_index(const char *varname)
{
   char *s = strchr(varname, '[');
   if (!s)
      return 0;
   char *se = strchr(varname, ']');
   if (!se) {
      error_message("%s: missing closing bracked in '%s'\n", __func__, varname);
      return -1;
   }
   *se = 0;
   ++s;
   int retval = 0;
   if (sscanf(s, "%d", &retval) != 1) {
      error_message("%s: unable to read number from '%s'\n", __func__, s);
      return -1;
   }
   return retval;
}

static
bool get_local_decl_var(const char *shader, nir_builder *b)
{
   bool retval = true;
   char interp[64];
   char vartype[64];
   char varname[64];
   int nread = sscanf(shader, "decl_var %63s %63s %63s",
                      interp, vartype, varname);

   if (nread != 3) {
      error_message("%s: Expect 3 values, read %d\n", __func__, nread);
      retval = false;
      goto fail;
   }

   const struct glsl_type *type = get_variable_type(vartype);
   if (!type) {
      retval = false;
      goto fail;
   }

   nir_local_variable_create(b->impl, type, varname);
fail:
   return retval;
}

static
bool get_local_decl_reg(const char *shader, nir_builder *b)
{
   bool retval = false;
   int  typesize =0;
   char vartype[64];
   char varname[64];

   int nread = sscanf(shader, "decl_reg %63s %d %63s",
                      vartype, &typesize, varname);

   if (nread != 3) {
      error_message("%s: Expect 3 values, read %d\n", __func__, nread);
      goto fail;
   }

   const struct glsl_type *type = get_variable_type(vartype);
   if (!type)
      goto fail;

   nir_register *reg = nir_local_reg_create(b->impl);
   reg->num_components = glsl_get_components(type);
   reg->bit_size = (uint8_t)typesize;

   int array_size = get_array_index(varname);
   if (array_size < 0)
      goto fail;

   reg->num_array_elems = array_size;

   retval = true;
fail:
   return retval;
}

static
bool get_global_decl_var(const char *shader, nir_builder *b)
{
   bool retval = false;
   char global_type[64];
   char interp[64];
   char vartype[64];
   char varname[64];
   char slot[64];
   char driver_loc[64];
   char binding_id[64];

   int nread = sscanf(shader, "decl_var %63s %63s %63s %63s (%63s %63s %63s",
                      global_type, interp, vartype, varname, slot, driver_loc,
                      binding_id);

   if (nread < 4) {
      error_message("%s: unable to read global variable type\n", __func__);
      goto fail;
   }

   char *coma = strchr(slot, ',');
   if (coma) *coma = 0;

   nir_variable_mode mode = get_variable_mode(global_type);
   if (!mode)
      goto fail;


   const struct glsl_type *type = get_variable_type(vartype);

   if (!type)
      goto fail;

   nir_variable *var =
         nir_variable_create(b->shader, mode, type, varname);

   if (mode != nir_var_system_value && nread < 7) {
      error_message("%s: unable to read global variable, expect 7 values got only %d\n", __func__, nread);
   }

   if (mode == nir_var_shader_in) {
      if (b->shader->info.stage == MESA_SHADER_VERTEX)
         var->data.location = get_vert_attr(slot);
      else
         var->data.location = get_varying(slot);

      b->shader->info.inputs_read |= 1ull << var->data.location;
   }

   if (mode == nir_var_shader_out) {
      if (b->shader->info.stage == MESA_SHADER_FRAGMENT)
         var->data.location = get_frag_out(slot);
         else
         var->data.location = get_varying(slot);

      b->shader->info.outputs_written |= 1ull << var->data.location;
   }

   if (mode == nir_var_shader_in || mode == nir_var_shader_out) {
      var->data.interpolation = get_interp(interp);
      var->data.driver_location = (unsigned)atoi(driver_loc);
   }

   if (mode == nir_var_uniform || mode == nir_var_mem_ubo) {
      var->data.location = atoi(slot);
      var->data.driver_location = (unsigned)atoi(driver_loc);
      var->data.binding  = atoi(binding_id);
   }

   retval = true;
fail:
   return retval;
}

static
nir_intrinsic_op get_intrinsic_opcode(const char *opstr)
{
   for (int i = 0; i < nir_num_intrinsics; ++i) {
      const nir_intrinsic_info *info =  &nir_intrinsic_infos[i];
      if (!strcmp(opstr, info->name))
         return i;
   }
   error_message("Opcode %s not found\n", opstr);
   return nir_num_intrinsics;
}

static nir_variable* find_var(struct exec_list *list, const char *name)
{
   nir_foreach_variable(var, list) {
      if (!strcmp(name, var->name))
         return var;
   }
   error_message("Variable %s not found\n", name);
   return NULL;
}

static
bool get_ssa_index(const char *buf, unsigned *value)
{
   while (buf[0] == ' ') ++buf;
   bool ssa = buf[0] == 's';
   assert(ssa);
   if (sscanf(buf, "ssa_%u", value) != 1) {
      error_message("Unable to extract index from %s\n", buf);
      return false;
   }
   return true;
}

static
nir_register *find_register(const char *namebuf,  const struct nir_function *func)
{
   assert(namebuf[0] == 'r');
   unsigned regindex = 0;
   if (sscanf(namebuf, "r%d", &regindex) == 1) {
      nir_foreach_register(reg, &func->impl->registers)
         if (reg->index == regindex)
            return reg;
   }
   error_message("Register %s not found\n", namebuf);
   return NULL;
}

static
nir_ssa_def *find_ssa_dest(const struct nir_function *func, const char *namebuf)
{
   unsigned idx = 0xffffffff;
   if (!get_ssa_index(namebuf, &idx))
      return false;

   nir_foreach_block(block, func->impl) {
      nir_foreach_instr(instr, block) {

         switch(instr->type) {
         case nir_instr_type_deref: {
            nir_deref_instr *di = nir_instr_as_deref(instr);
            if (di->dest.ssa.index == idx)
               return  &di->dest.ssa;
            break;
         }
         case nir_instr_type_intrinsic: {
            nir_intrinsic_instr *di = nir_instr_as_intrinsic(instr);
            if (di->dest.ssa.index == idx)
               return &di->dest.ssa;
            break;
         }
         case nir_instr_type_alu: {
            nir_alu_instr *di = nir_instr_as_alu(instr);
            if (di->dest.dest.ssa.index == idx)
               return &di->dest.dest.ssa;
            break;
         }
         case nir_instr_type_load_const: {
            nir_load_const_instr *di = nir_instr_as_load_const(instr);
            if (di->def.index == idx)
               return &di->def;
            break;
         }
         case nir_instr_type_tex: {
            nir_tex_instr *di = nir_instr_as_tex(instr);
            if (di->dest.ssa.index == idx)
               return &di->dest.ssa;
            break;
         }
         case nir_instr_type_phi: {
            nir_phi_instr *di = nir_instr_as_phi(instr);
            if (di->dest.ssa.index == idx)
               return &di->dest.ssa;
            break;
         }

         default:
            ;
         }
      }
   }
   return NULL;
}

static
int get_array_offset_and_register(const char *namebuf, char indirect[32])
{
   int retval = -1;
   char *start = strchr(namebuf, '[');
   if (!start) {
      error_message("Expect array access but git '%s'\n", namebuf);
      return -1;
   }
   ++start;
   char *end = strchr(namebuf, ']');
   if (!end) {
      error_message("Expect array access but git '%s'\n", namebuf);
      return -1;
   }
   *end = 0;


   if (indirect) {
      char *plus = strchr(start, '+');
      if (!plus) {
         indirect[0] = 0;
         sscanf(start, "%d", &retval);
      } else {
         sscanf(start, "%d + %s", &retval, indirect);
      }
   }
   return retval;
}

static
bool find_dest(nir_src *src, const struct nir_function *func, const char *namebuf)
{
   while (*namebuf == ' ') ++namebuf;

   if (namebuf[0] == 'r') {
      nir_register *reg = find_register(namebuf, func);
      if (reg) {
         *src = nir_src_for_reg(reg);
         if (reg->num_array_elems > 0) {
            char indirect[32];
            int base_offset = get_array_offset_and_register(namebuf, indirect);
            if (base_offset < 0)
               return false;
            src->reg.base_offset = base_offset;
            if (indirect[0] != 0) {
               src->reg.indirect = ralloc(func, nir_src);
               if (!find_dest(src->reg.indirect, func, indirect))
                  return false;
            }
         }
         return true;
      }
      return false;
   }

   if (namebuf[0] != 's') {
      error_message("Not a register and not ssa: %s\n", namebuf);
      return false;
   }
   nir_ssa_def *def = find_ssa_dest(func, namebuf);
   if (def) {
      *src = nir_src_for_ssa(def);
      return true;
   }
   return false;
}

static
void get_braced_range(char *shader, char **start, char **end)
{
   *start = strchr(shader, '(');
   *end = strchr(shader, ')');
   char *s = *start;
   **start = ' ';

   while (s != *end) {
      if (*s == ',') *s = ' ';
      ++s;
   }
   **end = ' ';
}

static
bool link_all_sources(char *shader, int num_srcs, nir_src *src, const struct nir_function *func)
{
   char *si = NULL;
   char *se = NULL;
   get_braced_range(shader, &si, &se);

   char namebuf[64];
   for (int i = 0; i < num_srcs; ++i) {
      if (sscanf(si, "%s", namebuf) != 1) {
         error_message("Unable to get name from '%s'\n", si);
         return false;
      }
      si += strlen(namebuf)+2;

      if (!find_dest(&src[i], func, namebuf)) {
         error_message("unable to find ssa register from %s\n", namebuf);
         return false;
      }
   }
   return true;
}

static
bool load_all_indices(char *shader, const nir_intrinsic_info *info, int *const_index)
{
   char *si = NULL;
   char *se = NULL;
   get_braced_range(shader, &si, &se);

   for (unsigned i = 0; i < info->num_indices; ++i) {
      int value = 0;
      if (sscanf(si, "%d", &value) != 1) {
         error_message("Unable to extract index nr %d from '%s'\n", i, si);
         return false;
      }

      const_index[i] = value;
      while (*si == ' ') ++si;
      while (*si >= '0' && *si <= '9' ) ++si;
   }
   return true;
}

typedef struct {
   const struct glsl_type *type;
   int compsize;
   const char *varname;
} ssa_params;

typedef struct {
   union {
      nir_register *reg;
      ssa_params ssa;
   };
   bool is_ssa;
   int array_index;
   nir_src *array_indirect;
} dest_params;

typedef bool (*add_op_cb)(dest_params *dest_descr, char *shader, nir_builder *b);

static
bool add_intrinsic_op(char *shader, nir_builder *b)
{
   bool retval = false;
   char opstr[64];
   if (sscanf(shader, "intrinsic %63s", opstr) != 1) {
      error_message("Unable to extract intrinsic op in %s\n", shader);
      return false;
   }

   nir_intrinsic_op op = get_intrinsic_opcode(opstr);
   if (op == nir_num_intrinsics) {
      error_message("Intrinsic op '%s' not found\n", opstr);
      retval = false;
      goto fail;
   }

   nir_intrinsic_instr *out_instr = nir_intrinsic_instr_create(b->shader, op);
   const nir_intrinsic_info *info =  &nir_intrinsic_infos[op];

   if (info->num_srcs > 0) {
      const struct nir_function *func =
            (const struct nir_function *)exec_list_get_head_const(&b->shader->functions);
      if (!link_all_sources(shader, info->num_srcs, out_instr->src, func)) {
         goto fail;
      }
   }

   if (info->num_indices > 0) {
      if (!load_all_indices(shader, info, out_instr->const_index)) {
         error_message("Loat all indices failed for %s\n", shader);
         goto fail;
      }
   }

   if (info->index_map[NIR_INTRINSIC_WRMASK]) {
      int wmask = out_instr->const_index[info->index_map[NIR_INTRINSIC_WRMASK] - 1];
      out_instr->num_components = 0;
      while (wmask > 0) {
         ++out_instr->num_components;
         wmask >>= 1;
      }
   }

   nir_builder_instr_insert(b, &out_instr->instr);

   retval = true;
fail:
   return retval;
}

static
bool add_intrinsic_op_with_dest(dest_params *dest_descr, char *shader, nir_builder *b)
{
   bool retval = false;
   char opstr[64];
   if (sscanf(shader, "%63s", opstr) != 1)
      return false;

   nir_intrinsic_op op = get_intrinsic_opcode(opstr);
   if (op == nir_num_intrinsics)
      goto fail;

   const nir_intrinsic_info *info =  &nir_intrinsic_infos[op];
   nir_intrinsic_instr *out_instr = nir_intrinsic_instr_create(b->shader, op);

   if (dest_descr->is_ssa) {
      nir_ssa_dest_init(&out_instr->instr, &out_instr->dest,
                        glsl_get_components(dest_descr->ssa.type), dest_descr->ssa.compsize, NULL);
      out_instr->num_components = (unsigned char)glsl_get_components(dest_descr->ssa.type);
      get_ssa_index(dest_descr->ssa.varname, &out_instr->dest.ssa.index);
   } else {
      out_instr->dest.reg.reg = dest_descr->reg;
   }

   if (info->num_srcs > 0) {
      const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&b->shader->functions);
      if (!link_all_sources(shader, info->num_srcs, out_instr->src, func)) {
         error_message("%s: link all sources failed\n", __func__);
         goto fail;
      }
   }

   if (info->num_indices > 0) {
      if (!load_all_indices(shader, info, out_instr->const_index)) {
         error_message("%s: load all indices failed\n", __func__);
         goto fail;
      }
   }

   nir_builder_instr_insert(b, &out_instr->instr);

   switch (op) {
   case nir_intrinsic_load_front_face:
         b->shader->info.system_values_read |= 1ull << SYSTEM_VALUE_FRONT_FACE;
      break;
   default:
      ;
   }

   retval = true;
fail:
   return retval;
}

static
bool add_load_const(dest_params *dest_descr, char *shader, nir_builder *b)
{
   bool retval = false;

   assert(dest_descr->is_ssa);
   int ncomps = glsl_get_components(dest_descr->ssa.type);

   nir_load_const_instr *out_instr = nir_load_const_instr_create(b->shader, ncomps, dest_descr->ssa.compsize);
   get_ssa_index(dest_descr->ssa.varname, &out_instr->def.index);

   char *si = strchr(shader, '(');
   if (!si) {
      error_message("%s: didn't find starting '('\n", __func__);
      goto fail;
   }
   ++si;

   for (int i = 0; i < ncomps; ++i) {
      uint32_t value;
      if (sscanf(si, "%x", &value) != 1) {
         error_message("%s: unable to read value from %s\n", __func__, si);
         goto fail;
      }

      /* skip comment */
      out_instr->value[i].u32 = value;
      si = strstr(si, "*/");
      if (si)
         si += 3;
   }

   nir_builder_instr_insert(b, &out_instr->instr);
   retval = true;
fail:
   return retval;
}

static nir_variable_mode split_name_and_mode(char *instring, char *deref_name)
{
   char locstr[64];
   nir_variable_mode mode = (nir_variable_mode)0;

   if (sscanf(instring, " &%63s (%63s", deref_name, locstr) != 2) {
      error_message("%s: Error parsing %s", __func__, instring);
      return mode;
   }
   return get_variable_mode(locstr);
}

static
nir_variable *get_variable(nir_builder *b, char *deref_name, nir_variable_mode mode)
{
   switch (mode) {
   case nir_var_shader_in:
      return find_var(&b->shader->inputs, deref_name);
      break;
   case nir_var_shader_out:
      return find_var(&b->shader->outputs, deref_name);
      break;
   case nir_var_uniform:
      return find_var(&b->shader->uniforms, deref_name);
      break;
   case nir_var_function_temp:
      return find_var(&b->impl->locals, deref_name);
      break;
   default:
      return NULL;
   }
}

static
nir_ssa_def *find_constant_with_value(const struct nir_function *func, int value, int comps,  nir_builder *b)
{
   nir_foreach_block(block, func->impl) {
      nir_foreach_instr(instr, block) {
         if (instr->type == nir_instr_type_load_const) {
            nir_load_const_instr *di = nir_instr_as_load_const(instr);
            if (di->value[0].i32 == value && di->def.num_components == comps)
               return &di->def;
         }
      }
   }

   return nir_imm_int(b, value);
}

static
bool add_deref_array(dest_params *dest_descr, char *shader, nir_builder *b)
{
   bool retval = false;
   char deref_name[64];
   char real_deref_name[64];
   char indirect_index[64];
   assert(dest_descr->is_ssa);
   int index;

   nir_variable_mode mode = split_name_and_mode(shader, deref_name);
   if (!mode)
      goto fail;

   assert(deref_name[0] == '(');

   char *deref_search_name = deref_name + 2; /* remove '(*' */
   if (!split_index(deref_search_name, real_deref_name, &index, indirect_index))
      goto fail;

   const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&b->shader->functions);

   nir_ssa_def *d = find_ssa_dest(func, real_deref_name);
   nir_ssa_def *idx = (index >= 0) ? find_constant_with_value(func, index, 1, b): find_ssa_dest(func, indirect_index);
   nir_deref_instr *v = nir_build_deref_array(b, nir_instr_as_deref(d->parent_instr), idx);
   get_ssa_index(dest_descr->ssa.varname, &v->dest.ssa.index);
   v->dest.ssa.num_components = (unsigned char)glsl_get_components(dest_descr->ssa.type);

   retval = true;
fail:
   return retval;
}

static
bool add_deref_var(dest_params *dest_descr, char *shader, nir_builder *b)
{
   bool retval = false;
   char deref_name[64];
   assert(dest_descr->is_ssa);

   nir_variable_mode mode = split_name_and_mode(shader, deref_name);
   if (!mode)
      goto fail;

   // find the variable in the according list

   nir_variable *var = get_variable(b, deref_name, mode);
   if (!var) {
      error_message("%s: Error derefereincing var %s\n", __func__, deref_name);
      return false;
   }

   nir_deref_instr *v = nir_build_deref_var(b, var);
   get_ssa_index(dest_descr->ssa.varname, &v->dest.ssa.index);
   v->dest.ssa.num_components = (unsigned char)glsl_get_components(dest_descr->ssa.type);

   retval = true;
fail:
   return retval;
}

static
int strip_modifiers(char **namebuf)
{
   int result = 0;
   char *has_neg = strchr(*namebuf, '-');
   if (has_neg) {
      *namebuf += 1;
      result |= 2;
   }

   char * has_abs = strstr(*namebuf, "abs(");
   if (has_abs) {
      *namebuf +=4;
      char *closing = strchr(*namebuf, ')');
      assert(closing);
      *closing = ' ';
      result |= 1;
   }
   return result;
}

static void extract_swizzle_and_remove_dot(char *namebuf, unsigned char *swizzle)
{
   char *dot = strchr(namebuf, '.');
   if (dot) {
      int pos = 0;

      *dot = ' ';
      ++dot;

      while (*dot && strchr("xyzw", *dot))  {
         switch (*dot) {
         case 'x': swizzle[pos++] = 0; break;
         case 'y': swizzle[pos++] = 1; break;
         case 'z': swizzle[pos++] = 2; break;
         case 'w': swizzle[pos++] = 3; break;
         }
         ++dot;
      }
      for (; pos < 4; ++pos)
         swizzle[pos] = 7;
   }
}

static bool
add_alu_op(dest_params *dest_descr, char *shader, int op, const nir_op_info *info, char *dest_modifier, nir_builder *b)
{
   bool retval = false;
   nir_alu_instr * out_instr = nir_alu_instr_create(b->shader, op);

   if (dest_descr->is_ssa) {
      nir_ssa_dest_init(&out_instr->instr, &out_instr->dest.dest,
                        glsl_get_components(dest_descr->ssa.type), dest_descr->ssa.compsize, NULL);
      out_instr->dest.write_mask = (1 << glsl_get_components(dest_descr->ssa.type)) - 1;
      get_ssa_index(dest_descr->ssa.varname, &out_instr->dest.dest.ssa.index);
   } else {
      out_instr->dest.dest.is_ssa = false;
      out_instr->dest.dest.reg.reg = dest_descr->reg;
      out_instr->dest.write_mask = (1 << dest_descr->reg->num_components) - 1;
      if (dest_descr->reg->num_array_elems > 0)
         out_instr->dest.dest.reg.base_offset = dest_descr->array_index;
      if (dest_descr->array_indirect)
         out_instr->dest.dest.reg.indirect = dest_descr->array_indirect;
   }

   if (dest_modifier && !strncmp(dest_modifier, "sat", 3)) {
      out_instr->dest.saturate = true;
   }

   char *si = shader;
   for (unsigned i = 0; i < info->num_inputs; ++i) {
      char *saveptr;
      char *namebuf = strtok_r(i == 0 ? si: NULL, ",", &saveptr);
      if (!namebuf) {
         error_message("Error reding src %d \n", i);
         goto fail;
      }
      while (*namebuf == ' ') ++namebuf;

      int flags = strip_modifiers(&namebuf);

      unsigned char swizzle[4] = {0,1,2,3};
      extract_swizzle_and_remove_dot(namebuf, swizzle); /* Clears the dot as side effect */

      const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&b->shader->functions);

      if (!find_dest(&out_instr->src[i].src, func, namebuf)) {
         error_message("Register '%s' not found in allocated destinations\n", namebuf);
         goto fail;
      }
      out_instr->src[i].abs = flags & 1;
      out_instr->src[i].negate = flags & 2;

      int last_swizzle = 0;
      for (int j = 0; j < 4; ++j) {
         if (swizzle[j] < 4)
            last_swizzle = swizzle[j];
         out_instr->src[i].swizzle[j] = last_swizzle;
      }
   }

   nir_builder_instr_insert(b, &out_instr->instr);

   retval = true;
fail:

   return retval;
}

typedef struct {
   nir_texop opcode;
   const char *name;
   int num_inputs;

} nir_texop_info;

static
nir_texop_info texop_info[] = {
   {nir_texop_tex, "tex", 1},
   {nir_texop_txb, "txb", 2},
   {nir_texop_txl, "txl", -1},
   {nir_texop_txd, "txd", -1},
   {nir_texop_txf, "txf", 2},
   {nir_texop_txf_ms, "txf_ms", -1},
   {nir_texop_txf_ms_mcs, "txf_ms_mcs", -1},
   {nir_texop_txs, "txs", -1},
   {nir_texop_lod, "lod", -1},
   {nir_texop_tg4, "tg4", -1},
   {nir_texop_query_levels, "query_levels", -1},
   {nir_texop_texture_samples, "texture_samples", -1},
   {nir_texop_samples_identical, "samples_identical", -1},
   {nir_texop_samples_identical + 1, NULL, -1}
};

static char * set_tex_index_value(nir_tex_instr *out_instr, char *namebuf, char *tail)
{
   int value = atoi(namebuf);
   char *name_start = strchr(tail, '(');
   if (!name_start)
      return NULL;

   char *name_end = strchr(tail, ')');
   if (!name_end)
      return NULL;
   ++name_end;

   if (!strncmp("(texture)", name_start, 9)) {
      out_instr->texture_index = value;
      return name_end;
   }

   if (!strncmp("(sampler)", name_start, 9)) {
      out_instr->sampler_index = value;
      return name_end;
   }
   return NULL;
}

static bool
add_tex_op(dest_params *params, char *shader, UNUSED int i, const nir_texop_info *info, nir_builder *b)
{
   bool retval = false;
   char namebuf[64];
   char *si = shader;
   unsigned isrc = 0;

   nir_src srcs[4];
   nir_tex_src_type src_type[4];

   while (*si != '\n' && *si != 0) {
      sscanf(si, "%63s", namebuf);
      if (namebuf[0] >= '0' && namebuf[0]<= '9')
         break;

      si += strlen(namebuf) + 1;
      unsigned char swizzle[4] = {0,1,2,3};
      extract_swizzle_and_remove_dot(namebuf, swizzle); /* Clears the dot as side effect */

      const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&b->shader->functions);

      if (!find_dest(&srcs[isrc], func, namebuf)) {
         error_message("Register '%s' not found in allocated destinations\n", namebuf);
         goto fail;
      }

      char *parenthesis = strchr(si, '(');
      if (!parenthesis) {
         error_message("%s: Did not find opening '(' in %s\n", __func__, si);
         goto fail;
      }
      si = parenthesis;

      if (!strncmp("(coord)", si, 7))
         src_type[isrc] = nir_tex_src_coord;
      else if (!strncmp("(comparator)", si, 12))
         src_type[isrc] = nir_tex_src_comparator;
      else if (!strncmp("(lod)", si, 5))
         src_type[isrc] = nir_tex_src_lod;
      else {
         error_message("%s: source type not supported %s\n", __func__, si);
         goto fail;
      }

      ++isrc;

      char *comma = strchr(si, ',');
      if (!comma) {
         error_message("%s: didn't find seperator ',' in %s\n", __func__, si);
         goto fail;
      }
      si = comma;
      ++si;
   }

   nir_tex_instr * out_instr = nir_tex_instr_create(b->shader, isrc);
   out_instr->op = info->opcode;
   out_instr->dest_type = nir_type_float;

   if (params->is_ssa) {
      nir_ssa_dest_init(&out_instr->instr, &out_instr->dest,
                        glsl_get_components(params->ssa.type), params->ssa.compsize, NULL);
      get_ssa_index(params->ssa.varname, &out_instr->dest.ssa.index);
   } else {
      out_instr->dest.is_ssa = false;
      out_instr->dest.reg.reg = params->reg;
      if (params->reg->num_array_elems > 0)
         out_instr->dest.reg.base_offset = params->array_index;
      if (params->array_indirect)
         out_instr->dest.reg.indirect = params->array_indirect;
   }

   for (unsigned k = 0; k < isrc; ++k) {
      out_instr->src[k].src = srcs[k];
      out_instr->src[k].src_type = src_type[k];

      if (src_type[k] == nir_tex_src_coord) {
         out_instr->sampler_dim = out_instr->src[k].src.ssa->num_components;
      }
   }

   while (*si != '\n' && *si != 0) {
      si += strlen(namebuf) + 1;
      si = set_tex_index_value(out_instr, namebuf, si);
      if (!si) {
         error_message("%s: Error setting tex index value\n", __func__);
         goto fail;
      }
      if (*si == ',')
         ++si;
      sscanf(si, "%63s", namebuf);
   }

   nir_builder_instr_insert(b, &out_instr->instr);

   retval = true;
fail:
   return retval;
}

static
struct nir_block *find_block(const struct nir_function *func, unsigned block_id)
{
   unsigned index = 0;
   nir_foreach_block(block, func->impl) {
      if (index == block_id)
         return block;
      ++index;
   }
   error_message("Block %d not found\n", block_id);
   return NULL;
}

static bool update_phi_nodes(block_tracker *bt)
{
   bool retval = false;

   while (!exec_list_is_empty(&bt->phi_handles)) {
      phi_handle *ph = (phi_handle *)exec_list_pop_head(&bt->phi_handles);

      nir_cursor cursor = nir_instr_remove(&ph->phi->instr);

      char *saveptr = NULL;

      char *block_id_str = strtok_r(ph->source_list_string, ":", &saveptr);
      if (!block_id_str) {
         error_message("%s: No block found in %s\n", __func__, ph->source_list_string);
         goto fail;
      }

      while (block_id_str) {
         int block_id = -1;
         char *regname = strtok_r(NULL, ",", &saveptr);
         if (!regname) {
            error_message("%s: No regname found for block id %s\n", __func__, block_id_str);
            goto fail;
         }

         if (sscanf(block_id_str, " block_%d", &block_id) != 1) {
            error_message("%s: Unable to extract block ID from '%s'\n", __func__, block_id_str);
            goto fail;
         }
         const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&bt->b->shader->functions);
         nir_ssa_def *src = find_ssa_dest(func, regname);

         nir_phi_src *phi_src  = ralloc(ph->phi, nir_phi_src);
         phi_src->pred = find_block(func, block_id);
         phi_src->src = nir_src_for_ssa(src);


         exec_list_push_tail(&ph->phi->srcs, &phi_src->node);
         block_id_str = strtok_r(NULL, ":", &saveptr);
      }
      free(ph->source_list_string);
      nir_instr_insert(cursor, &ph->phi->instr);
   }

   retval = true;
fail:
   return retval;
}

static bool
add_phi_node(dest_params *params, char *shader, block_tracker *bt)
{
   /* Phi nodes should contain only ssa */
   assert(params->is_ssa);

   nir_phi_instr *out_instr = nir_phi_instr_create(bt->b->shader);
   nir_ssa_dest_init(&out_instr->instr, &out_instr->dest,
                     glsl_get_components(params->ssa.type),
                     params->ssa.compsize, NULL);
   get_ssa_index(params->ssa.varname, &out_instr->dest.ssa.index);

   phi_handle *ph = rzalloc_size(bt->b->shader, sizeof(phi_handle));
   ph->phi = out_instr;
   ph->source_list_string = strdup(shader);
   exec_list_push_head(&bt->phi_handles, &ph->node);
   nir_builder_instr_insert(bt->b, &out_instr->instr);

   return true;
}


static
bool add_op_unknown(UNUSED dest_params *params, UNUSED char *shader, UNUSED nir_builder *b)
{
   return false;
}

typedef struct {
   const char *name;
   add_op_cb cb;
} op_table;

static op_table operation_table[] = {
   {"deref_var", add_deref_var},
   {"deref_array", add_deref_array},
   {"intrinsic", add_intrinsic_op_with_dest},
   {"load_const", add_load_const},
   {NULL, add_op_unknown},
};

static
bool add_op_with_dest(char *shader, block_tracker *bt, bool ssa_dest)
{
   bool retval = false;
   char varname[64];
   char opstr[64];
   dest_params dest_descr;
   memset(&dest_descr, 0, sizeof(dest_descr));

   char *right_side  = strchr(shader, '=');
   if (!right_side) {
      goto fini;
   }

   *right_side = 0;
   ++right_side;

   dest_descr.is_ssa = ssa_dest;
   if (ssa_dest) {
      char typestr[64];
      if (sscanf(shader,"%63s %d %63s ",  typestr, &dest_descr.ssa.compsize, varname) != 3)
         goto fini;
      dest_descr.ssa.type = get_variable_type(typestr);
      dest_descr.ssa.varname = varname;

      if (!dest_descr.ssa.type) {
         goto fini;
      }
   } else {
      char *varname = shader;
      while (*varname == ' ') ++varname;

      dest_descr.reg = find_register(shader, bt->b->impl->function);
      if (!dest_descr.reg)
         goto fini;

      if (dest_descr.reg->num_array_elems > 0) {
         char indirect[32];
         dest_descr.array_index = get_array_offset_and_register(varname, indirect);
         if (dest_descr.array_index < 0)
            goto fini;
         if (*indirect != 0) {
            dest_descr.array_indirect = ralloc(bt->b->impl,  nir_src);
            const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&bt->b->shader->functions);
            find_dest(dest_descr.array_indirect, func, indirect);
         } else {
            dest_descr.array_indirect = NULL;
         }
      }
   }

   shader = right_side;
   if (sscanf(shader," %s",  opstr) != 1) {
      error_message("%s: no opstring found in %s\n", __func__, shader);
      goto fini;
   }

   shader += strlen(opstr) + 1;

   op_table *ot = operation_table;

   /* Try special ops give in the table above */
   while (ot->name) {
      if (!strcmp(opstr, ot->name)) {
          retval = ot->cb(&dest_descr, shader, bt->b);
          goto fini;
      }
      ++ot;
   }

   char  *dest_modifier = strchr(opstr, '.');
   if (dest_modifier) {
      *dest_modifier = 0;
      ++dest_modifier;
   }

   /* Try ALU ops */
   for (int i = 0; i < nir_num_opcodes; ++i) {
      const nir_op_info *info = &nir_op_infos[i];
      if (info && info->name)
         if (!strcmp(info->name, opstr)) {
            retval = add_alu_op(&dest_descr, shader, i, info, dest_modifier, bt->b);
            goto fini;
         }
   }

   /* Try TEX ops */
   for (int i = 0; i <= nir_texop_samples_identical; ++i) {
      const nir_texop_info *info = &texop_info[i];
      if (info && info->name)
         if (!strcmp(info->name, opstr)) {
            if (info->num_inputs < 0) {
               error_message("TEX opcode '%s' not yet supported\n", info->name);
               goto fini;
            }
            retval = add_tex_op(&dest_descr, shader, i, info, bt->b);
         }
   }

   /* Try PHI node */
   if (!strncmp("phi", opstr, 3)) {
      retval = add_phi_node(&dest_descr, shader, bt);
   }

fini:
   if (!retval)
      error_message("nir_shader_from_string: Unsupported op '%s'\n", opstr);
   return retval;
}

#define NEXT_LINE_RET_IF_FAIL(S) S = next_line(S); if (!S) return false

static
bool get_header(char **sh, nir_builder *b)
{

   char *shader =*sh;
   int inputs = 0;
   int outputs = 0;
   int shared = 0;

   sscanf(shader, "inputs: %d", &inputs);
   shader = next_line(shader);
   if (!shader) return false;

   sscanf(shader, "outputs: %d", &outputs);
   NEXT_LINE_RET_IF_FAIL(shader);

   sscanf(shader, "uniforms: %d", &b->shader->num_uniforms);
   NEXT_LINE_RET_IF_FAIL(shader);

   sscanf(shader, "shared: %d", &shared);
   NEXT_LINE_RET_IF_FAIL(shader);

   /* skip over lines that are not var or function declaration */
   while (strncmp(shader, "decl", 4) && strncmp(shader, "impl", 4)) {
      NEXT_LINE_RET_IF_FAIL(shader);
   }

   while (!strncmp(shader, "decl_var", 8)) {
      if (!get_global_decl_var(shader, b))
         return false;
      NEXT_LINE_RET_IF_FAIL(shader);
   }

   *sh = shader;

   return true;
}

#undef NEXT_LINE_RET_IF_FAIL

static
bool start_block(UNUSED const char *shader, UNUSED int block_nr, UNUSED nir_builder *b)
{
   return true;
}

typedef struct {
   struct exec_node node;
   nir_cf_node *cf;
} cf_stack_element;

static
bool start_loop(const char *shader, block_tracker *bt)
{
   ++bt->nesting_level;
   nir_loop *nloop = nir_push_loop(bt->b);
   cf_stack_element *elm = rzalloc_size(NULL, sizeof(cf_stack_element));
   elm->cf = &nloop->cf_node;
   exec_list_push_head(&bt->cf_nodes, &elm->node);
   return true;
}


static
bool start_if(const char *shader, block_tracker *bt)
{
   bool retval = false;
   ++bt->nesting_level;

   const char *si = shader + 2;
   char namebuf[20];

   if (sscanf(si, "%19s", namebuf) != 1)
      goto fail;

   const struct nir_function *func = (const struct nir_function *)exec_list_get_head_const(&bt->b->shader->functions);
   nir_ssa_def *cond = find_ssa_dest(func, namebuf);
   if (!cond) {
      error_message("ssa_def '%s' not found in allocated destinations\n", namebuf);
      goto fail;
   }

   nir_if *nif = nir_push_if(bt->b, cond);
   cf_stack_element *elm = rzalloc_size(NULL, sizeof(cf_stack_element));
   elm->cf = &nif->cf_node;

   exec_list_push_head(&bt->cf_nodes, &elm->node);
   retval = true;
fail:
   return retval;
}

static
bool start_else(const char *shader, block_tracker *bt)
{
   /* Here I have to switch the current cf node to append to */
   assert(!exec_list_is_empty(&bt->cf_nodes));

   cf_stack_element *cf = (cf_stack_element *)exec_list_get_head(&bt->cf_nodes);
   nir_push_else(bt->b, nir_cf_node_as_if(cf->cf));

   return true;
}

static
bool emit_break(UNUSED const char *shader, block_tracker *bt)
{
   /* Here I have to switch the current cf node to append to */
   assert(!exec_list_is_empty(&bt->cf_nodes));

   nir_jump_instr *instr = nir_jump_instr_create(bt->b->shader, nir_jump_break);
   nir_builder_instr_insert(bt->b, &instr->instr);

   return true;
}

static
bool emit_continue(UNUSED const char *shader, block_tracker *bt)
{
   /* Here I have to switch the current cf node to append to */
   assert(!exec_list_is_empty(&bt->cf_nodes));

   nir_jump_instr *instr = nir_jump_instr_create(bt->b->shader, nir_jump_continue);
   nir_builder_instr_insert(bt->b, &instr->instr);

   return true;
}

static
bool end_block(block_tracker *bt)
{
  bool retval = true;

   --bt->nesting_level;
   if (!bt->nesting_level)
      return retval;

   assert(!exec_list_is_empty(&bt->cf_nodes));

   cf_stack_element *cf = (cf_stack_element *)exec_list_pop_head(&bt->cf_nodes);

   if (cf->cf->type == nir_cf_node_if)
      nir_pop_if(bt->b, nir_cf_node_as_if(cf->cf));
   else if (cf->cf->type == nir_cf_node_loop)
      nir_pop_loop(bt->b, nir_cf_node_as_loop(cf->cf));
   else
      retval = false;

   ralloc_free(cf);

   return retval;
}

static
char *get_next_line(char **saveptr)
{
   char *retval = NULL;
   do {
      retval = strtok_r(NULL, "\n", saveptr);
      if (!retval)
         break;

      bool has_comment;
      do  {
         has_comment = false;
         while (*retval && strchr("\t \r\n", *retval))
            ++retval;
         if (!strncmp(retval, "/*", 2)) {
            retval = strstr(retval, "*/");
            if (!retval)
               return NULL;
            retval+=2;
            has_comment = true;
         }
      } while (has_comment);

      if (!retval)
         break;
   } while (! *retval);
   return retval;
}

static
bool get_function(char **sh, nir_builder *b)
{
   char *saveptr = NULL;

   char *shader = strtok_r(*sh, "\n", &saveptr);
   int block_nr = 0;

   block_tracker bt = {
      .nesting_level = 1,
      .b=b
   };

#define NEXT_LINE_SKIP_WS_RET_IF_FAIL(line) \
   line = get_next_line(&saveptr); \
   if (!line) return false;

   exec_list_make_empty(&bt.cf_nodes);
   exec_list_make_empty(&bt.phi_handles);

   assert(!strncmp(shader, "impl main {", 9));

   NEXT_LINE_SKIP_WS_RET_IF_FAIL(shader);

   while (!strncmp(shader, "decl_var", 8)) {
      if (!get_local_decl_var(shader, b))
         return false;
      NEXT_LINE_SKIP_WS_RET_IF_FAIL(shader);
   }
   while (!strncmp(shader, "decl_reg", 8)) {
      if (!get_local_decl_reg(shader, b))
         return false;
      NEXT_LINE_SKIP_WS_RET_IF_FAIL(shader);
   }

   while (*shader) {
      if (!strncmp(shader, "block", 5)) {
         if (!start_block(shader, block_nr++, b))
            return false;
      } else if (!strncmp(shader, "vec", 3)) {
         if (!add_op_with_dest(shader, &bt, true))
            return false;
      } else if (!strncmp(shader, "intrinsic", 9)) {
         if (!add_intrinsic_op(shader, b))
            return false;
      } else if (*shader == 'r') {
         if (!add_op_with_dest(shader, &bt, false))
            return false;
      } else if (!strncmp(shader, "if", 2)) {
         if (!start_if(shader, &bt))
            return false;
      } else if (!strncmp(shader, "} else", 6)) {
         if (!start_else(shader, &bt))
            return false;
      } else if (!strncmp(shader, "loop {", 6)) {
         if (!start_loop(shader, &bt))
            return false;
      } else if (!strncmp(shader, "break", 5)) {
         if (!emit_break(shader, &bt))
            return false;
      } else if (!strncmp(shader, "continue", 5)) {
         if (!emit_continue(shader, &bt))
            return false;
      } else if (*shader == '}') {
         if (!end_block(&bt))
            return false;
         if (!bt.nesting_level)
            break;
      } else {
         if (!strncmp(shader, "/*", 2)) {
            shader = strstr(shader, "*/");
            if (shader) {
               shader +=2;
               while (*shader && *shader == ' ') ++shader;
            }
         }
         error_message("Unhandled line '%s'\n", shader);
      }

      NEXT_LINE_SKIP_WS_RET_IF_FAIL(shader);
   }

   if (!update_phi_nodes(&bt))
      return false;

   *sh = shader;
   return bt.nesting_level == 0;
}


static
gl_shader_stage get_stage(char *line)
{
   char shader_str[64];
   gl_shader_stage retval = -1;

   if (sscanf(line, "shader: %63s", shader_str) != 1) {
      fprintf(stderr, "Unable do read shader stage from line %s", line);
      assert(0); 
      return retval; 
   }


#define CHECK_STAGE(S, X) if (!strcmp(#X, S)) retval = X;

   
   CHECK_STAGE(shader_str, MESA_SHADER_VERTEX);
   CHECK_STAGE(shader_str, MESA_SHADER_FRAGMENT);
   CHECK_STAGE(shader_str, MESA_SHADER_TESS_CTRL);
   CHECK_STAGE(shader_str, MESA_SHADER_TESS_EVAL);
   CHECK_STAGE(shader_str, MESA_SHADER_GEOMETRY);
   CHECK_STAGE(shader_str, MESA_SHADER_COMPUTE);
   if (retval < 0) {
      fprintf(stderr, "Unable do detect shader stage of %s", shader_str);
      assert(0);
   }

   return retval;
}

nir_shader *nir_shader_from_string(const char *sh, const nir_shader_compiler_options *options)
{
   nir_builder b;
   nir_shader *retval = NULL;
   char *shader_dup = strdup(sh);
   char *shader = shader_dup;

   while (*shader == '\n' || *shader == ' ' || *shader =='\t')
      ++shader; 

   gl_shader_stage stage = get_stage(shader);
   nir_builder_init_simple_shader(&b, NULL, stage, options);
   shader = next_line(shader);
   if (!shader)
      goto fail;

   if (!get_header(&shader, &b))
      goto fail;

   /* Support only on function for now, so skip over until we find "main"*/

   while (strncmp(shader, "impl main", 9)) {
      shader = next_line(shader);
      if (!shader)
         goto fail;
   }

   if (!get_function(&shader, &b))
      goto fail;

   b.shader->num_inputs = exec_list_length(&b.shader->inputs);
   b.shader->num_outputs = exec_list_length(&b.shader->outputs);

   nir_index_ssa_defs(b.impl);
   retval = b.shader;


fail:
   if (!retval)
      ralloc_free(b.shader);
   free(shader_dup);
   return retval;
}
