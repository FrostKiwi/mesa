# Copyright © 2017 Intel Corporation

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

libintel_compiler_files = files(
  'brw_cfg.cpp',
  'brw_cfg.h',
  'brw_clip.h',
  'brw_clip_line.c',
  'brw_clip_point.c',
  'brw_clip_tri.c',
  'brw_clip_unfilled.c',
  'brw_clip_util.c',
  'brw_compile_clip.c',
  'brw_compile_sf.c',
  'brw_compiler.c',
  'brw_compiler.h',
  'brw_dead_control_flow.cpp',
  'brw_dead_control_flow.h',
  'brw_debug_recompile.c',
  'brw_disasm.c',
  'brw_disasm_info.c',
  'brw_disasm_info.h',
  'brw_eu.cpp',
  'brw_eu_compact.c',
  'brw_eu_defines.h',
  'brw_eu_emit.c',
  'brw_eu.h',
  'brw_eu_util.c',
  'brw_eu_validate.c',
  'brw_fs_bank_conflicts.cpp',
  'brw_fs_builder.h',
  'brw_fs_cmod_propagation.cpp',
  'brw_fs_combine_constants.cpp',
  'brw_fs_copy_propagation.cpp',
  'brw_fs.cpp',
  'brw_fs_cse.cpp',
  'brw_fs_dead_code_eliminate.cpp',
  'brw_fs_generator.cpp',
  'brw_fs.h',
  'brw_fs_live_variables.cpp',
  'brw_fs_live_variables.h',
  'brw_fs_lower_pack.cpp',
  'brw_fs_lower_regioning.cpp',
  'brw_fs_nir.cpp',
  'brw_fs_reg_allocate.cpp',
  'brw_fs_register_coalesce.cpp',
  'brw_fs_saturate_propagation.cpp',
  'brw_fs_scoreboard.cpp',
  'brw_fs_sel_peephole.cpp',
  'brw_fs_validate.cpp',
  'brw_fs_visitor.cpp',
  'brw_inst.h',
  'brw_interpolation_map.c',
  'brw_ir.h',
  'brw_ir_allocator.h',
  'brw_ir_analysis.h',
  'brw_ir_fs.h',
  'brw_ir_performance.h',
  'brw_ir_performance.cpp',
  'brw_ir_vec4.h',
  'brw_nir.h',
  'brw_nir.c',
  'brw_nir_analyze_boolean_resolves.c',
  'brw_nir_analyze_ubo_ranges.c',
  'brw_nir_attribute_workarounds.c',
  'brw_nir_lower_conversions.c',
  'brw_nir_lower_cs_intrinsics.c',
  'brw_nir_lower_alpha_to_coverage.c',
  'brw_nir_lower_image_load_store.c',
  'brw_nir_lower_mem_access_bit_sizes.c',
  'brw_nir_lower_scoped_barriers.c',
  'brw_nir_opt_peephole_ffma.c',
  'brw_nir_tcs_workarounds.c',
  'brw_nir_clamp_image_1d_2d_array_sizes.c',
  'brw_packed_float.c',
  'brw_predicated_break.cpp',
  'brw_reg.h',
  'brw_reg_type.c',
  'brw_reg_type.h',
  'brw_schedule_instructions.cpp',
  'brw_shader.cpp',
  'brw_shader.h',
  'brw_vec4_builder.h',
  'brw_vec4_cmod_propagation.cpp',
  'brw_vec4_copy_propagation.cpp',
  'brw_vec4.cpp',
  'brw_vec4_cse.cpp',
  'brw_vec4_dead_code_eliminate.cpp',
  'brw_vec4_generator.cpp',
  'brw_vec4_gs_visitor.cpp',
  'brw_vec4_gs_visitor.h',
  'brw_vec4.h',
  'brw_vec4_live_variables.cpp',
  'brw_vec4_live_variables.h',
  'brw_vec4_nir.cpp',
  'brw_vec4_gs_nir.cpp',
  'brw_vec4_reg_allocate.cpp',
  'brw_vec4_surface_builder.cpp',
  'brw_vec4_surface_builder.h',
  'brw_vec4_tcs.cpp',
  'brw_vec4_tcs.h',
  'brw_vec4_tes.cpp',
  'brw_vec4_tes.h',
  'brw_vec4_visitor.cpp',
  'brw_vec4_vs_visitor.cpp',
  'brw_vec4_vs.h',
  'brw_vue_map.c',
  'brw_wm_iz.cpp',
  'gen_eu_performance.h',
  'gen6_gs_visitor.cpp',
  'gen6_gs_visitor.h',
  'nir_to_ibc.c',
  'ibc.c',
  'ibc.h',
  'ibc_alu_ops.h',
  'ibc_assign_and_lower_accums.c',
  'ibc_assign_and_lower_flags.c',
  'ibc_assign_regs.c',
  'ibc_builder.h',
  'ibc_compute.c',
  'ibc_compile.h',
  'ibc_dominance.c',
  'ibc_eu_performance.c',
  'ibc_fragment.c',
  'ibc_to_binary.c',
  'ibc_live_intervals.c',
  'ibc_live_intervals.h',
  'ibc_lower_gather_ops.c',
  'ibc_lower_integer_multiplication.c',
  'ibc_lower_io_to_sends.c',
  'ibc_lower_overlapping_send_payloads.c',
  'ibc_lower_simd_width.c',
  'ibc_nir.h',
  'ibc_opt_copy_prop.c',
  'ibc_opt_cse.c',
  'ibc_opt_coalesce.c',
  'ibc_opt_dead_code.c',
  'ibc_opt_gather.c',
  'ibc_opt_halt.c',
  'ibc_opt_predicate.c',
  'ibc_print.c',
  'ibc_ra_reg_sets.c',
  'ibc_ra_reg_sets.h',
  'ibc_schedule_instructions.c',
  'ibc_split_regs.c',
  'ibc_tess_eval.c',
  'ibc_urb.c',
  'ibc_validate.c',
  'ibc_vertex.c',
)

brw_nir_trig = custom_target(
  'brw_nir_trig_workarounds.c',
  input : 'brw_nir_trig_workarounds.py',
  output : 'brw_nir_trig_workarounds.c',
  command : [
    prog_python, '@INPUT@',
    '-p', join_paths(meson.source_root(), 'src/compiler/nir/'),
  ],
  depend_files : nir_algebraic_py,
  capture : true,
)

ra_sets_build_args = []
dep_zstd_build = null_dep
dep_zlib_build = null_dep
if _zstd != 'disabled' and dep_zstd.found()
  dep_zstd_build = dependency('libzstd', required : _zstd == 'enabled', native : true)
  if dep_zstd_build.found()
    ra_sets_build_args += '-DHAVE_ZSTD_BUILD'
    ra_sets_ext = '.zstd'
  endif
endif

if _zstd == 'enabled' and not (dep_zstd.found() and dep_zstd_build.found())
  error('ZSTD required, but could not find both a build and host system version for IBC.')
endif

if not (dep_zstd_build.found() and dep_zstd.found())
  dep_zlib_build = dependency('zlib', version : '>= 1.2.3', fallback : ['zlib', 'zlib_dep'], native : true)
  ra_sets_ext = '.zlib'
endif

exe_ibc_build_ra_reg_sets = executable(
  'ibc_build_ra_reg_sets',
  [
    'ibc_build_ra_reg_sets.c',
    'ibc_ra_reg_sets.c',
    'ibc_ra_reg_sets.h',
  ],
  c_args : [pre_args],  # needed even though this is a build machine target
  include_directories : [inc_include, inc_src, inc_mapi, inc_mesa, inc_intel],
  # because this is a build machine target we need pre_args here
  c_args : [pre_args, ra_sets_build_args],
  dependencies : [idep_mesautil_build, dep_zlib_build, dep_zstd_build],
  native : true,
)

if build_machine.system() == 'big'
  error('Building IBC RA reg sets on big-endian is not supported.')
endif

if not host_machine.cpu_family().startswith('x86')
  error('Unsupported on non-x86 platforms')
endif
_generated_regsets = []
foreach s : ['8', '16', '32']
  _n = 'ibc_ra_reg_sets_prebuilt' + s
  _generated_regsets += custom_target(
    _n + ra_sets_ext,
    output : [_n + ra_sets_ext],
    command : [
      exe_ibc_build_ra_reg_sets, '@OUTPUT@', s,
    ],
  )
endforeach

reg_sets = custom_target(
  'ibc_generated_ra_reg_sets.h',
  command : [
    prog_python, files_xxd, '@OUTPUT@', '@INPUT@', '-b',
   '-n', 'ra_reg_sets8',
   '-n', 'ra_reg_sets16',
   '-n', 'ra_reg_sets32',
  ],
  input : _generated_regsets,
  output : 'ibc_generated_ra_reg_sets.h',
  depend_files : files_xxd,
)

libintel_compiler = static_library(
  'intel_compiler',
  [
    libintel_compiler_files, brw_nir_trig, ir_expression_operation_h,
    reg_sets,
  ],
  include_directories : [inc_include, inc_src, inc_mapi, inc_mesa, inc_gallium, inc_intel],
  c_args : [no_override_init_args, ra_sets_build_args, '-DIBC_HAVE_PREBUILT_REG_SETS'],
  gnu_symbol_visibility : 'hidden',
  dependencies : [idep_nir_headers, dep_zlib, dep_zstd, dep_zstd],
  build_by_default : false,
)

if with_tests
  # The last two tests are not C++ or gtest, pre comment in autotools make
  foreach t : ['fs_cmod_propagation', 'fs_copy_propagation',
               'fs_saturate_propagation', 'vf_float_conversions',
               'vec4_register_coalesce', 'vec4_copy_propagation',
               'vec4_cmod_propagation', 'vec4_dead_code_eliminate',
               'eu_compact', 'eu_validate', 'fs_scoreboard']
    test(
      t,
      executable(
        [t, ir_expression_operation_h],
        'test_@0@.cpp'.format(t),
        include_directories : [inc_include, inc_src, inc_mapi, inc_mesa, inc_gallium, inc_intel],
        link_with : [
          libintel_compiler, libintel_common, libintel_dev, libisl,
        ],
        dependencies : [idep_gtest, idep_nir, idep_mesautil],
      ),
      suite : ['intel'],
    )
  endforeach
endif
