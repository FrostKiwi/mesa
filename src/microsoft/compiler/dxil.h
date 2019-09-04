/*
 * Copyright 2019 Collabora Ltd.
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

#ifndef DXIL_H
#define DXIL_H

struct dxil_features {
   unsigned doubles : 1,
            cs_4x_raw_sb : 1,
            uavs_at_every_stage : 1,
            use_64uavs : 1,
            min_precision : 1,
            dx11_1_double_extensions : 1,
            dx11_1_shader_extensions : 1,
            dx9_comparison_filtering : 1,
            tiled_resources : 1,
            stencil_ref : 1,
            inner_coverage : 1,
            typed_uav_load_additional_formats : 1,
            rovs : 1,
            array_layer_from_vs_or_ds : 1,
            wave_ops : 1,
            int64_ops : 1,
            view_id : 1,
            barycentrics : 1,
            native_low_precision : 1,
            shading_rate : 1,
            raytracing_tier_1_1 : 1,
            sampler_feedback;
};

enum dxil_shader_kind {
   DXIL_PIXEL_SHADER = 0,
   DXIL_VERTEX_SHADER = 1,
   DXIL_GEOMETRY_SHADER = 2,
   DXIL_HULL_SHADER = 3,
   DXIL_DOMAIN_SHADER = 4,
   DXIL_COMPUTE_SHADER = 5,
};

#endif
