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

#include "d3d12_surface.h"

#include "util/u_inlines.h"
#include "util/u_memory.h"

static struct pipe_surface *
d3d12_create_surface(struct pipe_context *context,
                     struct pipe_resource *resource,
                     const struct pipe_surface *surf_tpl)
{
   unsigned int level = surf_tpl->u.tex.level;

   struct d3d12_surface *surface = CALLOC_STRUCT(d3d12_surface);
   if (!surface)
      return NULL;

   pipe_resource_reference(&surface->base.texture, resource);
   pipe_reference_init(&surface->base.reference, 1);
   surface->base.context = context;
   surface->base.format = surf_tpl->format;
   surface->base.width = u_minify(resource->width0, level);
   surface->base.height = u_minify(resource->height0, level);
   surface->base.u.tex.level = level;
   surface->base.u.tex.first_layer = surf_tpl->u.tex.first_layer;
   surface->base.u.tex.last_layer = surf_tpl->u.tex.last_layer;

   return &surface->base;
}

static void
d3d12_surface_destroy(struct pipe_context *context,
                      struct pipe_surface *surface)
{
   pipe_resource_reference(&surface->texture, NULL);
   FREE(surface);
}

void
d3d12_context_surface_init(struct pipe_context *context)
{
   context->create_surface = d3d12_create_surface;
   context->surface_destroy = d3d12_surface_destroy;
}
