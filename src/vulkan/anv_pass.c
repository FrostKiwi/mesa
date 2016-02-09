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

static VkResult
pass_init_attachments(struct anv_render_pass *pass,
                      const VkRenderPassCreateInfo *info,
                      const VkAllocationCallbacks *alloc)
{
   size_t size;

   pass->attachment_count = info->attachmentCount;
   pass->attachments = NULL;

   if (info->attachmentCount == 0)
      return VK_SUCCESS;

   size = info->attachmentCount * sizeof(pass->attachments[0]);
   pass->attachments = anv_alloc(alloc, size, 8,
                                 VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (!pass->attachments)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   for (uint32_t i = 0; i < info->attachmentCount; ++i) {
      const VkAttachmentDescription *desc = &info->pAttachments[i];
      struct anv_render_pass_attachment *a = &pass->attachments[i];

      a->format = anv_format_for_vk_format(desc->format);
      a->samples = desc->samples;
      a->clear_aspects = 0;
      a->first_subpass = UINT32_MAX;

      if (anv_format_is_color(a->format)) {
         /* color attachment */
         if (desc->loadOp == VK_ATTACHMENT_LOAD_OP_CLEAR) {
            a->clear_aspects |= VK_IMAGE_ASPECT_COLOR_BIT;
         }
      } else {
         /* depthstencil attachment */
         if (a->format->has_depth &&
             desc->loadOp == VK_ATTACHMENT_LOAD_OP_CLEAR) {
            a->clear_aspects |= VK_IMAGE_ASPECT_DEPTH_BIT;
         }
         if (a->format->has_stencil &&
             desc->stencilLoadOp == VK_ATTACHMENT_LOAD_OP_CLEAR) {
            a->clear_aspects |= VK_IMAGE_ASPECT_STENCIL_BIT;
         }
      }
   }

   /* For each attachment, find the first subpass that reads from or writes to
    * it. Since we walk the list of subpasses in reverse order, the "last"
    * subpass that uses an attachment is actually the first.
    */
   for (uint32_t s = info->subpassCount; s > 0; --s) {
      const VkSubpassDescription *desc = &info->pSubpasses[s - 1];

      #define USE(attachment) \
         do { \
            if (attachment != VK_ATTACHMENT_UNUSED) \
               pass->attachments[attachment].first_subpass = s - 1; \
         } while (0)

      for (uint32_t j = 0; j < desc->inputAttachmentCount; ++j) {
         USE(desc->pInputAttachments[j].attachment);
      }

      for (uint32_t j = 0; j < desc->colorAttachmentCount; ++j) {
         USE(desc->pColorAttachments[j].attachment);
      }

      if (desc->pResolveAttachments) {
         for (uint32_t j = 0; j < desc->colorAttachmentCount; ++j) {
            USE(desc->pResolveAttachments[j].attachment);
         }
      }

      if (desc->pDepthStencilAttachment)
         USE(desc->pDepthStencilAttachment->attachment);

      #undef USE
   }

   return VK_SUCCESS;
}

static VkResult
subpass_init_attachments(const VkAttachmentReference *attachment_refs,
                         uint32_t attachment_len,
                         const VkAllocationCallbacks *alloc,
                         uint32_t **attachments)
{
   if (attachment_len == 0) {
      *attachments = NULL;
      return VK_SUCCESS;
   }

   size_t size = attachment_len * sizeof((*attachments)[0]);
   *attachments = anv_alloc(alloc, size, 8,
                            VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (*attachments == NULL)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   for (uint32_t i = 0; i < attachment_len; ++i) {
      (*attachments)[i] = attachment_refs[i].attachment;
   }

   return VK_SUCCESS;
}

static bool
subpass_desc_has_resolve(const VkSubpassDescription *desc)
{
   if (!desc->pResolveAttachments)
      return false;

   for (uint32_t i = 0; i < desc->colorAttachmentCount; ++i) {
      if (desc->pResolveAttachments[i].attachment != VK_ATTACHMENT_UNUSED)
         return true;
   }

   return false;
}

static void
subpass_finish(struct anv_subpass *subpass,
               const VkAllocationCallbacks *alloc)
{
   anv_free(alloc, subpass->input_attachments);
   anv_free(alloc, subpass->color_attachments);
   anv_free(alloc, subpass->resolve_attachments);
}

static VkResult
subpass_init(struct anv_subpass *subpass,
             const VkRenderPassCreateInfo *info,
             uint32_t subpass_index,
             const VkAllocationCallbacks *alloc)

{
   const VkSubpassDescription *subpass_desc = &info->pSubpasses[subpass_index];
   uint32_t resolve_count = 0;
   VkResult res;

   subpass->input_count = subpass_desc->inputAttachmentCount;
   res = subpass_init_attachments(subpass_desc->pInputAttachments,
                                  subpass_desc->inputAttachmentCount, alloc,
                                  &subpass->input_attachments);
   if (res != VK_SUCCESS)
      goto fail_input_attachments;

   subpass->color_count = subpass_desc->colorAttachmentCount;
   res = subpass_init_attachments(subpass_desc->pColorAttachments,
                                  subpass_desc->colorAttachmentCount, alloc,
                                  &subpass->color_attachments);
   if (res != VK_SUCCESS)
      goto fail_color_attachments;

   if (subpass_desc_has_resolve(subpass_desc))
      resolve_count = subpass_desc->colorAttachmentCount;

   res = subpass_init_attachments(subpass_desc->pResolveAttachments,
                                  resolve_count, alloc,
                                  &subpass->resolve_attachments);
   if (res != VK_SUCCESS)
      goto fail_resolve_attachments;

   subpass->depth_stencil_attachment = VK_ATTACHMENT_UNUSED;
   if (subpass_desc->pDepthStencilAttachment) {
      subpass->depth_stencil_attachment =
         subpass_desc->pDepthStencilAttachment->attachment;
   }

   return VK_SUCCESS;

fail_resolve_attachments:
   anv_free(alloc, subpass->color_attachments);
fail_color_attachments:
   anv_free(alloc, subpass->input_attachments);
fail_input_attachments:
   return res;
}

static void
pass_finish_subpasses(struct anv_render_pass *pass,
                      const VkAllocationCallbacks *alloc)
{
   for (uint32_t i = 0; i < pass->subpass_count; ++i) {
      subpass_finish(&pass->subpasses[i], alloc);
   }

   anv_free(alloc, pass->subpasses);
}

static VkResult
pass_init_subpasses(struct anv_render_pass *pass,
                    const VkRenderPassCreateInfo *info,
                    const VkAllocationCallbacks *alloc)
{
   VkResult res;
   size_t size;

   pass->subpass_count = 0;
   pass->subpasses = NULL;

   if (info->subpassCount == 0)
      return VK_SUCCESS;

   size = info->subpassCount * sizeof(pass->subpasses[0]);
   pass->subpasses = anv_alloc(alloc, size, 8,
                               VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (!pass->subpasses)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   for (uint32_t i = 0; i < info->subpassCount; ++i) {
      res = subpass_init(&pass->subpasses[i], info, i, alloc);
      if (res != VK_SUCCESS)
         goto fail;
      ++pass->subpass_count;
   }

   return VK_SUCCESS;

fail:
   pass_finish_subpasses(pass, alloc);
   return res;
}

VkResult anv_CreateRenderPass(
    VkDevice                                    device_h,
    const VkRenderPassCreateInfo*               info,
    const VkAllocationCallbacks*                pAllocator,
    VkRenderPass*                               pRenderPass)
{
   ANV_FROM_HANDLE(anv_device, device, device_h);
   const VkAllocationCallbacks *alloc = pAllocator ? pAllocator
                                                   : &device->alloc;
   struct anv_render_pass *pass;
   VkResult res;

   pass = anv_alloc(alloc, sizeof(*pass), 8,
                    VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (!pass)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   res = pass_init_attachments(pass, info, alloc);
   if (res != VK_SUCCESS)
      goto fail_attachments;

   res = pass_init_subpasses(pass, info, alloc);
   if (res != VK_SUCCESS)
      goto fail_subpasses;

   *pRenderPass = anv_render_pass_to_handle(pass);

   return VK_SUCCESS;

fail_subpasses:
   anv_free(alloc, pass->attachments);
fail_attachments:
   anv_free(alloc, pass);
   return res;
}

void anv_DestroyRenderPass(
    VkDevice                                    _device,
    VkRenderPass                                _pass,
    const VkAllocationCallbacks*                pAllocator)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_render_pass, pass, _pass);
   const VkAllocationCallbacks *alloc = pAllocator ? pAllocator
                                                   : &device->alloc;

   pass_finish_subpasses(pass, alloc);
   anv_free(alloc, pass->attachments);
   anv_free(alloc, pass);
}

void anv_GetRenderAreaGranularity(
    VkDevice                                    device,
    VkRenderPass                                renderPass,
    VkExtent2D*                                 pGranularity)
{
   *pGranularity = (VkExtent2D) { 1, 1 };
}
