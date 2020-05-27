#ifndef D3D12_TRANSITION_FLAGS_H
#define D3D12_TRANSITION_FLAGS_H

typedef enum _SubresourceTransitionFlags
{
   SubresourceTransitionFlags_None = 0,
   SubresourceTransitionFlags_TransitionPreDraw = 1,
   SubresourceTransitionFlags_StateMatchExact = 4,
   SubresourceTransitionFlags_ForceExplicitState = 8,
   SubresourceTransitionFlags_NotUsedInCommandListIfNoStateChange = 0x10,
} SubresourceTransitionFlags;
DEFINE_ENUM_FLAG_OPERATORS(SubresourceTransitionFlags);

#endif // D3D12_TRANSITION_FLAGS_H