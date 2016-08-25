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

#include "util/mesa-sha1.h"
#include "util/hash_table.h"
#include "util/debug.h"
#include "anv_private.h"

static size_t
anv_shader_bin_size(const struct anv_pipeline_bind_map *bind_map)
{
   return sizeof(struct anv_shader_bin) +
          bind_map->surface_count * sizeof(struct anv_pipeline_binding) +
          bind_map->sampler_count * sizeof(struct anv_pipeline_binding);
}

struct anv_shader_bin *
anv_shader_bin_create(struct anv_device *device,
                      gl_shader_stage stage,
                      const unsigned char *src_sha1,
                      const void *kernel, size_t kernel_size,
                      const struct brw_stage_prog_data *prog_data,
                      const struct anv_pipeline_bind_map *bind_map)
{
   const size_t size = anv_shader_bin_size(bind_map);

   struct anv_shader_bin *shader =
      anv_alloc(&device->alloc, size, 8, VK_SYSTEM_ALLOCATION_SCOPE_DEVICE);
   if (!shader)
      return NULL;

   shader->ref_cnt = 1;

   shader->stage = stage;

   shader->kernel =
      anv_state_pool_alloc(&device->instruction_state_pool, kernel_size, 64);
   memcpy(shader->kernel.map, kernel, kernel_size);
   shader->kernel_size = kernel_size;

   switch (stage) {
   case MESA_SHADER_VERTEX:
      shader->prog_data.vs = *(struct brw_vs_prog_data *)prog_data;
      break;
   case MESA_SHADER_GEOMETRY:
      shader->prog_data.gs = *(struct brw_gs_prog_data *)prog_data;
      break;
   case MESA_SHADER_TESS_CTRL:
      shader->prog_data.tcs = *(struct brw_tcs_prog_data *)prog_data;
      break;
   case MESA_SHADER_TESS_EVAL:
      shader->prog_data.tes = *(struct brw_tes_prog_data *)prog_data;
      break;
   case MESA_SHADER_FRAGMENT:
      shader->prog_data.fs = *(struct brw_wm_prog_data *)prog_data;
      break;
   case MESA_SHADER_COMPUTE:
      shader->prog_data.cs = *(struct brw_cs_prog_data *)prog_data;
      break;
   default:
      unreachable("Invalid shader stage");
   }

   shader->bind_map.surface_count = bind_map->surface_count;
   shader->bind_map.sampler_count = bind_map->sampler_count;
   shader->bind_map.image_count = bind_map->image_count;

   struct anv_pipeline_binding *bindings = shader->bindings;

   shader->bind_map.surface_to_descriptor = bindings;
   typed_memcpy(shader->bind_map.surface_to_descriptor,
                bind_map->surface_to_descriptor, bind_map->surface_count);
   bindings += bind_map->surface_count;

   shader->bind_map.sampler_to_descriptor = bindings;
   typed_memcpy(shader->bind_map.sampler_to_descriptor,
                bind_map->sampler_to_descriptor, bind_map->sampler_count);
   bindings += bind_map->sampler_count;

   if (src_sha1)
      memcpy(shader->src_sha1, src_sha1, sizeof(shader->src_sha1));

   return shader;
}

void
anv_shader_bin_destroy(struct anv_device *device,
                       struct anv_shader_bin *shader)
{
   assert(shader->ref_cnt == 0);
   anv_state_pool_free(&device->instruction_state_pool, shader->kernel);
   anv_free(&device->alloc, shader);
}

/* Remaining work:
 *
 * - Compact binding table layout so it's tight and not dependent on
 *   descriptor set layout.
 *
 * - Review prog_data struct for size and cacheability: struct
 *   brw_stage_prog_data has binding_table which uses a lot of uint32_t for 8
 *   bit quantities etc; param, pull_param, and image_params are pointers, we
 *   just need the compation map. use bit fields for all bools, eg
 *   dual_src_blend.
 */

void
anv_hash_shader(unsigned char *hash, const void *key, size_t key_size,
                struct anv_shader_module *module,
                const char *entrypoint,
                const struct anv_pipeline_layout *pipeline_layout,
                const VkSpecializationInfo *spec_info)
{
   struct mesa_sha1 *ctx;

   ctx = _mesa_sha1_init();
   _mesa_sha1_update(ctx, key, key_size);
   _mesa_sha1_update(ctx, module->sha1, sizeof(module->sha1));
   _mesa_sha1_update(ctx, entrypoint, strlen(entrypoint));
   if (pipeline_layout) {
      _mesa_sha1_update(ctx, pipeline_layout->sha1,
                        sizeof(pipeline_layout->sha1));
   }
   /* hash in shader stage, pipeline layout? */
   if (spec_info) {
      _mesa_sha1_update(ctx, spec_info->pMapEntries,
                        spec_info->mapEntryCount * sizeof spec_info->pMapEntries[0]);
      _mesa_sha1_update(ctx, spec_info->pData, spec_info->dataSize);
   }
   _mesa_sha1_final(ctx, hash);
}

static struct anv_shader_bin *
anv_pipeline_cache_search_unlocked(struct anv_pipeline_cache *cache,
                                   const unsigned char *sha1)
{
   if (cache->cache == NULL)
      return NULL;

   struct hash_entry *entry = _mesa_hash_table_search(cache->cache, sha1);
   if (entry)
      return entry->data;
   else
      return NULL;
}

struct anv_shader_bin *
anv_pipeline_cache_search(struct anv_pipeline_cache *cache,
                          const unsigned char *sha1)
{
   struct anv_shader_bin *bin;

   pthread_mutex_lock(&cache->mutex);

   bin = anv_pipeline_cache_search_unlocked(cache, sha1);

   pthread_mutex_unlock(&cache->mutex);

   /* We increment refcount before handing it to the caller */
   if (bin)
      anv_shader_bin_ref(bin);

   return bin;
}

static struct anv_shader_bin *
anv_pipeline_cache_add_kernel(struct anv_pipeline_cache *cache,
                              gl_shader_stage stage,
                              const unsigned char *src_sha1,
                              const void *kernel,
                              size_t kernel_size,
                              const struct brw_stage_prog_data *prog_data,
                              const struct anv_pipeline_bind_map *bind_map)
{
   assert(cache->cache);

   /* Before uploading, check again that another thread didn't upload this
    * shader while we were compiling it.
    */
   if (src_sha1) {
      struct anv_shader_bin *cached =
         anv_pipeline_cache_search_unlocked(cache, src_sha1);
      if (cached)
         return cached;
   }

   struct anv_shader_bin *bin =
      anv_shader_bin_create(cache->device, stage, src_sha1,
                            kernel, kernel_size, prog_data, bind_map);
   if (!bin)
      return NULL;

   _mesa_hash_table_insert(cache->cache, bin->src_sha1, bin);

   return bin;
}

struct anv_shader_bin *
anv_pipeline_cache_upload_kernel(struct anv_pipeline_cache *cache,
                                 gl_shader_stage stage,
                                 const unsigned char *src_sha1,
                                 const void *kernel,
                                 size_t kernel_size,
                                 const struct brw_stage_prog_data *prog_data,
                                 const struct anv_pipeline_bind_map *bind_map)
{
   if (cache->cache) {
      pthread_mutex_lock(&cache->mutex);

      struct anv_shader_bin *bin =
         anv_pipeline_cache_add_kernel(cache, stage, src_sha1,
                                       kernel, kernel_size, prog_data, bind_map);

      pthread_mutex_unlock(&cache->mutex);

      /* We increment refcount before handing it to the caller */
      anv_shader_bin_ref(bin);

      return bin;
   } else {
      /* In this case, we're not caching it so the caller owns it entirely */
      return anv_shader_bin_create(cache->device, stage, src_sha1,
                                   kernel, kernel_size, prog_data, bind_map);
   }
}

struct cache_header {
   uint32_t header_size;
   uint32_t header_version;
   uint32_t vendor_id;
   uint32_t device_id;
   uint8_t  uuid[VK_UUID_SIZE];
};

static void
anv_pipeline_cache_load(struct anv_pipeline_cache *cache,
                        const void *data, size_t size)
{
   struct anv_device *device = cache->device;
   struct cache_header header;
   uint8_t uuid[VK_UUID_SIZE];

   if (cache->cache == NULL)
      return;

   if (size < sizeof(header))
      return;
   memcpy(&header, data, sizeof(header));
   if (header.header_size < sizeof(header))
      return;
   if (header.header_version != VK_PIPELINE_CACHE_HEADER_VERSION_ONE)
      return;
   if (header.vendor_id != 0x8086)
      return;
   if (header.device_id != device->chipset_id)
      return;
   anv_device_get_cache_uuid(uuid);
   if (memcmp(header.uuid, uuid, VK_UUID_SIZE) != 0)
      return;

   void *end = (void *) data + size;
   void *p = (void *) data + header.header_size;

   /* Count is the total number of valid entries */
   uint32_t count;
   if (p + sizeof(count) >= end)
      return;
   memcpy(&count, p, sizeof(count));
   p += align_u32(sizeof(count), 8);

   for (uint32_t i = 0; i < count; i++) {
      struct anv_shader_bin *entry = p;
      if (p + sizeof(entry) >= end)
         return;
      const size_t entry_size = anv_shader_bin_size(&entry->bind_map);

      struct anv_pipeline_bind_map bind_map = entry->bind_map;
      bind_map.surface_to_descriptor = entry->bindings;
      bind_map.sampler_to_descriptor = entry->bindings + bind_map.surface_count;
      p += align_u32(entry_size, 8);

      void *kernel = p;
      p += align_u32(entry->kernel_size, 8);
      if (p >= end)
         break;

      anv_pipeline_cache_add_kernel(cache, entry->stage, entry->src_sha1,
                                    kernel, entry->kernel_size,
                                    &entry->prog_data.base, &bind_map);
   }
}

static uint32_t
sha1_hash_func(const void *key)
{
   return _mesa_hash_data(key, 20);
}

static bool
sha1_compare_func(const void *a, const void *b)
{
   return memcmp(a, b, 20) == 0;
}

VkResult anv_CreatePipelineCache(
    VkDevice                                    _device,
    const VkPipelineCacheCreateInfo*            pCreateInfo,
    const VkAllocationCallbacks*                pAllocator,
    VkPipelineCache*                            pPipelineCache)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   struct anv_pipeline_cache *cache;

   assert(pCreateInfo->sType == VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO);
   assert(pCreateInfo->flags == 0);

   cache = anv_alloc2(&device->alloc, pAllocator,
                       sizeof(*cache), 8,
                       VK_SYSTEM_ALLOCATION_SCOPE_OBJECT);
   if (cache == NULL)
      return vk_error(VK_ERROR_OUT_OF_HOST_MEMORY);

   cache->device = device;
   pthread_mutex_init(&cache->mutex, NULL);

   if (env_var_as_boolean("ANV_ENABLE_PIPELINE_CACHE", true)) {
      cache->cache = _mesa_hash_table_create(NULL, sha1_hash_func,
                                             sha1_compare_func);
   } else {
      cache->cache = NULL;
   }

   if (pCreateInfo->initialDataSize > 0)
      anv_pipeline_cache_load(cache,
                              pCreateInfo->pInitialData,
                              pCreateInfo->initialDataSize);

   *pPipelineCache = anv_pipeline_cache_to_handle(cache);

   return VK_SUCCESS;
}

void anv_DestroyPipelineCache(
    VkDevice                                    _device,
    VkPipelineCache                             _cache,
    const VkAllocationCallbacks*                pAllocator)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_pipeline_cache, cache, _cache);

   pthread_mutex_destroy(&cache->mutex);

   if (cache->cache) {
      /* This is a bit unfortunate.  In order to keep things from randomly
       * going away, the shader cache has to hold a reference to all shader
       * binaries it contains.  We unref them when we destroy the cache.
       */
      struct hash_entry *entry;
      hash_table_foreach(cache->cache, entry)
         anv_shader_bin_unref(device, entry->data);

      _mesa_hash_table_destroy(cache->cache, NULL);
   }

   anv_free2(&device->alloc, pAllocator, cache);
}

VkResult anv_GetPipelineCacheData(
    VkDevice                                    _device,
    VkPipelineCache                             _cache,
    size_t*                                     pDataSize,
    void*                                       pData)
{
   ANV_FROM_HANDLE(anv_device, device, _device);
   ANV_FROM_HANDLE(anv_pipeline_cache, cache, _cache);
   struct cache_header *header;

   if (pData == NULL) {
      size_t size = sizeof(*header) + sizeof(uint32_t);

      if (cache->cache) {
         struct hash_entry *entry;
         hash_table_foreach(cache->cache, entry) {
            struct anv_shader_bin *bin = entry->data;
            size += anv_shader_bin_size(&bin->bind_map);
            size += bin->kernel_size;
         }
      }

      *pDataSize = size;
      return VK_SUCCESS;
   }

   if (*pDataSize < sizeof(*header)) {
      *pDataSize = 0;
      return VK_INCOMPLETE;
   }

   void *p = pData, *end = pData + *pDataSize;
   header = p;
   header->header_size = sizeof(*header);
   header->header_version = VK_PIPELINE_CACHE_HEADER_VERSION_ONE;
   header->vendor_id = 0x8086;
   header->device_id = device->chipset_id;
   anv_device_get_cache_uuid(header->uuid);
   p += header->header_size;

   uint32_t *count = p;
   p += align_u32(sizeof(*count), 8);
   *count = 0;

   if (cache->cache) {
      struct hash_entry *entry;
      hash_table_foreach(cache->cache, entry) {
         struct anv_shader_bin *bin = entry->data;
         const size_t bin_size = anv_shader_bin_size(&bin->bind_map);
         if (p + bin_size + bin->kernel_size >= end)
            break;

         memcpy(p, bin, bin_size);
         p += align_u32(bin_size, 8);
         memcpy(p, bin->kernel.map, bin->kernel_size);
         p += align_u32(bin->kernel_size, 8);

         (*count)++;
      }
   }

   *pDataSize = p - pData;

   return VK_SUCCESS;
}

VkResult anv_MergePipelineCaches(
    VkDevice                                    _device,
    VkPipelineCache                             destCache,
    uint32_t                                    srcCacheCount,
    const VkPipelineCache*                      pSrcCaches)
{
   ANV_FROM_HANDLE(anv_pipeline_cache, dst, destCache);

   if (!dst->cache)
      return VK_SUCCESS;

   for (uint32_t i = 0; i < srcCacheCount; i++) {
      ANV_FROM_HANDLE(anv_pipeline_cache, src, pSrcCaches[i]);
      if (!src->cache)
         continue;

      struct hash_entry *entry;
      hash_table_foreach(src->cache, entry) {
         struct anv_shader_bin *bin = entry->data;
         if (_mesa_hash_table_search(dst->cache, bin->src_sha1))
            continue;

         anv_shader_bin_ref(bin);
         _mesa_hash_table_insert(dst->cache, bin->src_sha1, bin);
      }
   }

   return VK_SUCCESS;
}
