/*
 * Copyright © 2015 Intel Corporation
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
 */

#include "anv_private.h"

VkResult anv_CreateRenderPass(
    VkDevice                                    _device,
    const VkRenderPassCreateInfo*               pCreateInfo,
    const VkAllocationCallbacks*                pAllocator,
    VkRenderPass*                               pRenderPass)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   struct anv_render_pass *pass;
   size_t size;
   size_t attachments_offset;

   assert(pCreateInfo->sType == VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO);

   size = sizeof(*pass);
   size += pCreateInfo->subpassCount * sizeof(pass->subpasses[0]);
   attachments_offset = size;
   size += pCreateInfo->attachmentCount * sizeof(pass->attachments[0]);

   pass = vk_alloc2(&device->alloc, pAllocator, size, 8,
                     VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (pass == NULL)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   /* Clear the subpasses along with the parent pass. This required because
    * each array member of anv_subpass must be a valid pointer if not NULL.
    */
   memset(pass, 0, size);
   pass->attachment_count = pCreateInfo->attachmentCount;
   pass->subpass_count = pCreateInfo->subpassCount;
   pass->attachments = (void *) pass + attachments_offset;

   pass->subpass_usages =
      vk_zalloc2(&device->alloc, pAllocator,
                 pass->subpass_count * pass->attachment_count *
                                       sizeof(*pass->subpass_usages),
                 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (pass->subpass_usages == NULL)
      goto fail_pass;

   enum anv_subpass_usage *usages = pass->subpass_usages;
   for (uint32_t i = 0; i < pCreateInfo->attachmentCount; i++) {
      struct anv_render_pass_attachment *att = &pass->attachments[i];

      att->format = pCreateInfo->pAttachments[i].format;
      att->samples = pCreateInfo->pAttachments[i].samples;
      att->usage = 0;
      att->load_op = pCreateInfo->pAttachments[i].loadOp;
      att->store_op = pCreateInfo->pAttachments[i].storeOp;
      att->stencil_load_op = pCreateInfo->pAttachments[i].stencilLoadOp;
      att->initial_layout = pCreateInfo->pAttachments[i].initialLayout;
      att->final_layout = pCreateInfo->pAttachments[i].finalLayout;
      att->subpass_usage = usages;
      usages += pass->subpass_count;
   }

   uint32_t subpass_attachment_count = 0;
   VkAttachmentReference *p;
   for (uint32_t i = 0; i < pCreateInfo->subpassCount; i++) {
      const VkSubpassDescription *desc = &pCreateInfo->pSubpasses[i];

      subpass_attachment_count +=
      pass->subpasses[i].attachment_count =
         desc->inputAttachmentCount +
         desc->colorAttachmentCount +
         (desc->pResolveAttachments ? desc->colorAttachmentCount : 0) +
         (desc->pDepthStencilAttachment != NULL);
   }

   pass->subpass_attachments =
      vk_alloc2(&device->alloc, pAllocator,
                 subpass_attachment_count * sizeof(VkAttachmentReference), 8,
                 VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (pass->subpass_attachments == NULL)
      goto fail_subpass_usages;

   bool has_color = false, has_depth = false, has_input = false;
   p = pass->subpass_attachments;
   for (uint32_t i = 0; i < pCreateInfo->subpassCount; i++) {
      const VkSubpassDescription *desc = &pCreateInfo->pSubpasses[i];
      struct anv_subpass *subpass = &pass->subpasses[i];

      subpass->input_count = desc->inputAttachmentCount;
      subpass->color_count = desc->colorAttachmentCount;
      subpass->attachments = p;

      if (desc->inputAttachmentCount > 0) {
         subpass->input_attachments = p;
         p += desc->inputAttachmentCount;

         for (uint32_t j = 0; j < desc->inputAttachmentCount; j++) {
            uint32_t a = desc->pInputAttachments[j].attachment;
            subpass->input_attachments[j] = desc->pInputAttachments[j];
            if (a != VK_ATTACHMENT_UNUSED) {
               has_input = true;
               pass->attachments[a].usage |= VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT;
               pass->attachments[a].subpass_usage[i] |= ANV_SUBPASS_USAGE_INPUT;
               pass->attachments[a].last_subpass_idx = i;

               if (desc->pDepthStencilAttachment &&
                   a == desc->pDepthStencilAttachment->attachment)
                  subpass->has_ds_self_dep = true;
            }
         }
      }

      if (desc->colorAttachmentCount > 0) {
         subpass->color_attachments = p;
         p += desc->colorAttachmentCount;

         for (uint32_t j = 0; j < desc->colorAttachmentCount; j++) {
            uint32_t a = desc->pColorAttachments[j].attachment;
            subpass->color_attachments[j] = desc->pColorAttachments[j];
            if (a != VK_ATTACHMENT_UNUSED) {
               has_color = true;
               pass->attachments[a].usage |= VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT;
               pass->attachments[a].subpass_usage[i] |= ANV_SUBPASS_USAGE_DRAW;
               pass->attachments[a].last_subpass_idx = i;
            }
         }
      }

      subpass->has_resolve = false;
      if (desc->pResolveAttachments) {
         subpass->resolve_attachments = p;
         p += desc->colorAttachmentCount;

         for (uint32_t j = 0; j < desc->colorAttachmentCount; j++) {
            uint32_t a = desc->pResolveAttachments[j].attachment;
            subpass->resolve_attachments[j] = desc->pResolveAttachments[j];
            if (a != VK_ATTACHMENT_UNUSED) {
               subpass->has_resolve = true;
               uint32_t color_att = desc->pColorAttachments[j].attachment;
               pass->attachments[color_att].usage |=
                  VK_IMAGE_USAGE_TRANSFER_SRC_BIT;
               pass->attachments[a].usage |= VK_IMAGE_USAGE_TRANSFER_DST_BIT;

               pass->attachments[color_att].subpass_usage[i] |=
                  ANV_SUBPASS_USAGE_RESOLVE_SRC;
               pass->attachments[a].subpass_usage[i] |=
                  ANV_SUBPASS_USAGE_RESOLVE_DST;
               pass->attachments[a].last_subpass_idx = i;
            }
         }
      }

      if (desc->pDepthStencilAttachment) {
         uint32_t a = desc->pDepthStencilAttachment->attachment;
         *p++ = subpass->depth_stencil_attachment =
            *desc->pDepthStencilAttachment;
         if (a != VK_ATTACHMENT_UNUSED) {
            has_depth = true;
            pass->attachments[a].usage |=
               VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT;
            pass->attachments[a].subpass_usage[i] |= ANV_SUBPASS_USAGE_DRAW;
            pass->attachments[a].last_subpass_idx = i;
         }
      } else {
         subpass->depth_stencil_attachment.attachment = VK_ATTACHMENT_UNUSED;
         subpass->depth_stencil_attachment.layout = VK_IMAGE_LAYOUT_UNDEFINED;
      }
   }

   pass->subpass_flushes =
      vk_zalloc2(&device->alloc, pAllocator,
                 (pass->subpass_count + 1) * sizeof(*pass->subpass_flushes),
                 8, VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (pass->subpass_flushes == NULL)
      goto fail_subpass_attachments;

   for (uint32_t i = 0; i < pCreateInfo->dependencyCount; i++) {
      const VkSubpassDependency *dep = &pCreateInfo->pDependencies[i];
      if (dep->srcSubpass != VK_SUBPASS_EXTERNAL) {
         pass->subpass_flushes[dep->dstSubpass] |=
            anv_pipe_invalidate_bits_for_access_flags(dep->dstAccessMask);
      }

      if (dep->dstSubpass != VK_SUBPASS_EXTERNAL) {
         pass->subpass_flushes[dep->srcSubpass + 1] |=
            anv_pipe_flush_bits_for_access_flags(dep->srcAccessMask);
      }
   }

   /* From the Vulkan 1.0.39 spec:
    *
    *    If there is no subpass dependency from VK_SUBPASS_EXTERNAL to the
    *    first subpass that uses an attachment, then an implicit subpass
    *    dependency exists from VK_SUBPASS_EXTERNAL to the first subpass it is
    *    used in. The subpass dependency operates as if defined with the
    *    following parameters:
    *
    *    VkSubpassDependency implicitDependency = {
    *        .srcSubpass = VK_SUBPASS_EXTERNAL;
    *        .dstSubpass = firstSubpass; // First subpass attachment is used in
    *        .srcStageMask = VK_PIPELINE_STAGE_TOP_OF_PIPE_BIT;
    *        .dstStageMask = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;
    *        .srcAccessMask = 0;
    *        .dstAccessMask = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT |
    *                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
    *        .dependencyFlags = 0;
    *    };
    *
    *    Similarly, if there is no subpass dependency from the last subpass
    *    that uses an attachment to VK_SUBPASS_EXTERNAL, then an implicit
    *    subpass dependency exists from the last subpass it is used in to
    *    VK_SUBPASS_EXTERNAL. The subpass dependency operates as if defined
    *    with the following parameters:
    *
    *    VkSubpassDependency implicitDependency = {
    *        .srcSubpass = lastSubpass; // Last subpass attachment is used in
    *        .dstSubpass = VK_SUBPASS_EXTERNAL;
    *        .srcStageMask = VK_PIPELINE_STAGE_ALL_COMMANDS_BIT;
    *        .dstStageMask = VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT;
    *        .srcAccessMask = VK_ACCESS_INPUT_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_COLOR_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT |
    *                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_READ_BIT |
    *                         VK_ACCESS_DEPTH_STENCIL_ATTACHMENT_WRITE_BIT;
    *        .dstAccessMask = 0;
    *        .dependencyFlags = 0;
    *    };
    *
    * We could implement this by walking over all of the attachments and
    * subpasses and checking to see if any of them don't have an external
    * dependency.  Or, we could just be lazy and add a couple extra flushes.
    * We choose to be lazy.
    */
   if (has_input) {
      pass->subpass_flushes[0] |=
         ANV_PIPE_TEXTURE_CACHE_INVALIDATE_BIT;
   }
   if (has_color) {
      pass->subpass_flushes[pass->subpass_count] |=
         ANV_PIPE_RENDER_TARGET_CACHE_FLUSH_BIT;
   }
   if (has_depth) {
      pass->subpass_flushes[pass->subpass_count] |=
         ANV_PIPE_DEPTH_CACHE_FLUSH_BIT;
   }

   *pRenderPass = anv_render_pass_to_handle(pass);

   return VK_SUCCESS;

fail_subpass_attachments:
   vk_free2(&device->alloc, pAllocator, pass->subpass_attachments);
fail_subpass_usages:
   vk_free2(&device->alloc, pAllocator, pass->subpass_usages);
fail_pass:
   vk_free2(&device->alloc, pAllocator, pass);

   return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);
}

void anv_DestroyRenderPass(
    VkDevice                                    _device,
    VkRenderPass                                _pass,
    const VkAllocationCallbacks*                pAllocator)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_render_pass, pass, _pass);

   if (!pass)
      return;

   vk_free2(&device->alloc, pAllocator, pass->subpass_attachments);
   vk_free2(&device->alloc, pAllocator, pass->subpass_usages);
   vk_free2(&device->alloc, pAllocator, pass);
}

void anv_GetRenderAreaGranularity(
    VkDevice                                    device,
    VkRenderPass                                renderPass,
    VkExtent2D*                                 pGranularity)
{
   ANV_FROM_HANDLE(anv_render_pass, pass, renderPass);

   /* This granularity satisfies HiZ fast clear alignment requirements
    * for all sample counts.
    */
   for (unsigned i = 0; i < pass->subpass_count; ++i) {
      if (pass->subpasses[i].depth_stencil_attachment.attachment !=
          VK_ATTACHMENT_UNUSED) {
         *pGranularity = (VkExtent2D) { .width = 8, .height = 4 };
         return;
      }
   }

   *pGranularity = (VkExtent2D) { 1, 1 };
}
