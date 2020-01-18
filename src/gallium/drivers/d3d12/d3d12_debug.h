#ifndef D3D12_DEBUG_H
#define D3D12_DEBUG_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define D3D12_DEBUG_VERBOSE      1
#define D3D12_DEBUG_OPENGL21     2
#define D3D12_DEBUG_EXPERIMENTAL 4
#define D3D12_DEBUG_DXIL         8

extern uint32_t d3d12_debug;

#ifdef __cplusplus
}
#endif

#endif
