/*
 * Copyright © Microsoft Corporation
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
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

#include "D3D12ResourceState.h"

//----------------------------------------------------------------------------------------------------------------------------------
CDesiredResourceState::SubresourceInfo const& CDesiredResourceState::GetSubresourceInfo(UINT SubresourceIndex) const
{
   if (AreAllSubresourcesSame())
   {
      SubresourceIndex = 0;
   }
   return m_spSubresourceInfo[SubresourceIndex];
}

//----------------------------------------------------------------------------------------------------------------------------------
void CDesiredResourceState::UpdateSubresourceState(unsigned SubresourceIndex, SubresourceInfo const& info)
{
   assert(SubresourceIndex < m_spSubresourceInfo.size());
   if (m_spSubresourceInfo[SubresourceIndex].State == UNKNOWN_RESOURCE_STATE ||
      info.State == UNKNOWN_RESOURCE_STATE ||
      IsD3D12WriteState(info.State, SubresourceTransitionFlags_None))
   {
      m_spSubresourceInfo[SubresourceIndex] = info;
   }
   else
   {
      // Accumulate read state state bits
      m_spSubresourceInfo[SubresourceIndex].State |= info.State;
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
void CDesiredResourceState::SetResourceState(SubresourceInfo const & Info)
{
   m_bAllSubresourcesSame = true;
   UpdateSubresourceState(0, Info);
}
    
//----------------------------------------------------------------------------------------------------------------------------------
void CDesiredResourceState::SetSubresourceState(UINT SubresourceIndex, SubresourceInfo const & Info)
{
   if (m_bAllSubresourcesSame && m_spSubresourceInfo.size() > 1)
   {
      std::fill(m_spSubresourceInfo.begin() + 1, m_spSubresourceInfo.end(), m_spSubresourceInfo[0]);
      m_bAllSubresourcesSame = false;
   }
   if (m_spSubresourceInfo.size() == 1)
   {
      SubresourceIndex = 0;
   }
   UpdateSubresourceState(SubresourceIndex, Info);
}

//----------------------------------------------------------------------------------------------------------------------------------
void CDesiredResourceState::Reset()
{
   SetResourceState(SubresourceInfo{});
}

//----------------------------------------------------------------------------------------------------------------------------------
void CCurrentResourceState::ConvertToSubresourceTracking()
{
   if (m_bAllSubresourcesSame && m_spLogicalState.size() > 1)
   {
      std::fill(m_spLogicalState.begin() + 1, m_spLogicalState.end(), m_spLogicalState[0]);
      m_bAllSubresourcesSame = false;
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
CCurrentResourceState::CCurrentResourceState(UINT SubresourceCount, bool bSimultaneousAccess)
   : m_bSimultaneousAccess(bSimultaneousAccess)
   , m_spLogicalState(SubresourceCount)
{
   m_spLogicalState[0] = LogicalState{};
}

//----------------------------------------------------------------------------------------------------------------------------------
D3D12_RESOURCE_STATES CCurrentResourceState::StateIfPromoted(D3D12_RESOURCE_STATES State, UINT SubresourceIndex, SubresourceTransitionFlags Flags)
{
   D3D12_RESOURCE_STATES Result = D3D12_RESOURCE_STATE_COMMON;

   if((Flags & SubresourceTransitionFlags_ForceExplicitState) != SubresourceTransitionFlags_ForceExplicitState && (
         m_bSimultaneousAccess ||
         !!(State & (
            D3D12_RESOURCE_STATE_PIXEL_SHADER_RESOURCE | 
            D3D12_RESOURCE_STATE_NON_PIXEL_SHADER_RESOURCE | 
            D3D12_RESOURCE_STATE_COPY_SOURCE | 
            D3D12_RESOURCE_STATE_COPY_DEST))))
   {
      auto CurState = GetLogicalSubresourceState(SubresourceIndex);

      // If the current state is COMMON...
      if(CurState.State == D3D12_RESOURCE_STATE_COMMON)
      {
         // ...then promotion is allowed
         Result = State;
      }
      // If the current state is a read state resulting from previous promotion...
      else if(CurState.IsPromotedState && !!(CurState.State & D3D12_RESOURCE_STATE_GENERIC_READ))
      {
         // ...then (accumulated) promotion is allowed
         Result = State |= CurState.State;
      }
   }

   return Result;
}



//----------------------------------------------------------------------------------------------------------------------------------
void CCurrentResourceState::SetLogicalResourceState(LogicalState const& State)
{
   m_bAllSubresourcesSame = true;
   m_spLogicalState[0] = State;
}

//----------------------------------------------------------------------------------------------------------------------------------
void CCurrentResourceState::SetLogicalSubresourceState(UINT SubresourceIndex, LogicalState const& State)
{
   ConvertToSubresourceTracking();
   m_spLogicalState[SubresourceIndex] = State;
}

//----------------------------------------------------------------------------------------------------------------------------------
auto CCurrentResourceState::GetLogicalSubresourceState(UINT SubresourceIndex) const -> LogicalState const&
{
   if (AreAllSubresourcesSame())
   {
      SubresourceIndex = 0;
   }
   return m_spLogicalState[SubresourceIndex];
}

//----------------------------------------------------------------------------------------------------------------------------------
void CCurrentResourceState::Reset()
{
   m_bAllSubresourcesSame = true;
   m_spLogicalState[0] = LogicalState{};
}

//----------------------------------------------------------------------------------------------------------------------------------
ResourceStateManager::ResourceStateManager()
{
   InitializeListHead(&m_TransitionListHead);
   // Reserve some space in these vectors upfront. Values are arbitrary.
   m_vResourceBarriers.reserve(50);
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::TransitionResource(TransitionableResourceState& Resource,
                                                CDesiredResourceState::SubresourceInfo const& State)
{
   Resource.m_DesiredState.SetResourceState(State);
   if (!Resource.IsTransitionPending())
   {
      InsertHeadList(&m_TransitionListHead, &Resource.m_TransitionListEntry);
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::TransitionSubresource(TransitionableResourceState& Resource,
                                                   UINT SubresourceIndex,
                                                   CDesiredResourceState::SubresourceInfo const& State)
{
   Resource.m_DesiredState.SetSubresourceState(SubresourceIndex, State);
   if (!Resource.IsTransitionPending())
   {
      InsertHeadList(&m_TransitionListHead, &Resource.m_TransitionListEntry);
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::ApplyResourceTransitionsPreamble()
{
   m_vResourceBarriers.clear();
}

//----------------------------------------------------------------------------------------------------------------------------------
/*static*/ bool ResourceStateManager::TransitionRequired(D3D12_RESOURCE_STATES CurrentState, D3D12_RESOURCE_STATES& DestinationState, SubresourceTransitionFlags Flags)
{
   // An exact match never needs a transition.
   if (CurrentState == DestinationState)
   {
      return false;
   }

   // Not an exact match, but an exact match required, so do the transition.
   if ((Flags & SubresourceTransitionFlags_StateMatchExact) != SubresourceTransitionFlags_None)
   {
      return true;
   }

   if (
      CurrentState == D3D12_RESOURCE_STATE_COMMON ||
      DestinationState == D3D12_RESOURCE_STATE_COMMON)
   {
      return true;
   }

   // Current state already contains the destination state, we're good.
   if ((CurrentState & DestinationState) == DestinationState)
   {
      DestinationState = CurrentState;
      return false;
   }

   // If the transition involves a write state, then the destination should just be the requested destination.
   // Otherwise, accumulate read states to minimize future transitions (by triggering the above condition).
   if (!IsD3D12WriteState(DestinationState, SubresourceTransitionFlags_None) &&
      !IsD3D12WriteState(CurrentState, SubresourceTransitionFlags_None))
   {
      DestinationState |= CurrentState;
   }
   return true;
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::AddCurrentStateUpdate(TransitionableResourceState& Resource,
                                                 CCurrentResourceState& CurrentState,
                                                 UINT SubresourceIndex,
                                                 const CCurrentResourceState::LogicalState &NewLogicalState)
{
   if (SubresourceIndex == D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES)
   {
      CurrentState.SetLogicalResourceState(NewLogicalState);
   }
   else
   {
      CurrentState.SetLogicalSubresourceState(SubresourceIndex, NewLogicalState);
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
auto ResourceStateManager::ProcessTransitioningResource(ID3D12Resource* pTransitioningResource,
                                                            TransitionableResourceState& TransitionableResourceState,
                                                            CCurrentResourceState& CurrentState,
                                                            UINT NumTotalSubresources,
                                                            UINT64 ExecutionId,
                                                            bool bIsPreDraw) -> TransitionResult
{
   // By default, assume that the resource is fully processed by this routine.
   TransitionResult result = TransitionResult::Remove;

   // Figure out the set of subresources that are transitioning
   auto& DestinationState = TransitionableResourceState.m_DesiredState;
   bool bAllSubresourcesAtOnce = CurrentState.AreAllSubresourcesSame() && DestinationState.AreAllSubresourcesSame();

   D3D12_RESOURCE_BARRIER TransitionDesc;
   ZeroMemory(&TransitionDesc, sizeof(TransitionDesc));
   TransitionDesc.Type = D3D12_RESOURCE_BARRIER_TYPE_TRANSITION;
   TransitionDesc.Transition.pResource = pTransitioningResource;

   UINT numSubresources = bAllSubresourcesAtOnce ? 1 : NumTotalSubresources;
   for (UINT i = 0; i < numSubresources; ++i)
   {
      CDesiredResourceState::SubresourceInfo SubresourceDestinationInfo = DestinationState.GetSubresourceInfo(i);
      TransitionDesc.Transition.Subresource = bAllSubresourcesAtOnce ? D3D12_RESOURCE_BARRIER_ALL_SUBRESOURCES : i;

      // Is this subresource relevant for the current transition?
      if ((SubresourceDestinationInfo.Flags & SubresourceTransitionFlags_TransitionPreDraw) != SubresourceTransitionFlags_None &&
            !bIsPreDraw)
      {
            // Nope, we'll go to the next subresource, and also indicate to leave this resource in the transition list so that
            // we come back to it on the next draw operation.
            result = TransitionResult::Keep;
            continue;
      }

      // Is this subresource currently being used, or is it just being iterated over?
      D3D12_RESOURCE_STATES after = SubresourceDestinationInfo.State;
      SubresourceTransitionFlags Flags = SubresourceDestinationInfo.Flags;
      if (after == UNKNOWN_RESOURCE_STATE)
      {
            // This subresource doesn't have any transition requested - move on to the next.
            continue;
      }

      ProcessTransitioningSubresourceExplicit(
         CurrentState,
         i,
         SubresourceDestinationInfo,
         after,
         TransitionableResourceState,
         TransitionDesc,
         Flags,
         ExecutionId); // throw( bad_alloc )
   }

   CDesiredResourceState::SubresourceInfo UnknownDestinationState = {};
   UnknownDestinationState.State = UNKNOWN_RESOURCE_STATE;
   UnknownDestinationState.Flags = SubresourceTransitionFlags_None;

   // Update destination states.
   if (result == TransitionResult::Remove) // We're done
   {
      // Coalesce destination state to ensure that it's set for the entire resource.
      DestinationState.SetResourceState(UnknownDestinationState);
   }
   else if (!bIsPreDraw)
   {
      // There must be some subresource which was pending a draw transition, but not one that transitioned this time.
      // Make sure all *other* subresources have their pending transitions cleared.
      assert(!DestinationState.AreAllSubresourcesSame() ||
            (DestinationState.GetSubresourceInfo(0).Flags & SubresourceTransitionFlags_TransitionPreDraw) != SubresourceTransitionFlags_None);

      bAllSubresourcesAtOnce = DestinationState.AreAllSubresourcesSame();
      numSubresources = bAllSubresourcesAtOnce ? 1 : NumTotalSubresources;

      for (UINT i = 0; i < numSubresources; ++i)
      {
         if ((DestinationState.GetSubresourceInfo(i).Flags & SubresourceTransitionFlags_TransitionPreDraw) == SubresourceTransitionFlags_None)
         {
            if (bAllSubresourcesAtOnce)
            {
               DestinationState.SetResourceState(UnknownDestinationState);
            }
            else
            {
               DestinationState.SetSubresourceState(i, UnknownDestinationState);
            }
         }
      }
   }

   return result;
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::ProcessTransitioningSubresourceExplicit(
   CCurrentResourceState& CurrentState,
   UINT SubresourceIndex,
   CDesiredResourceState::SubresourceInfo& SubresourceDestinationInfo,
   D3D12_RESOURCE_STATES after,
   TransitionableResourceState& TransitionableResourceState,
   D3D12_RESOURCE_BARRIER& TransitionDesc,
   SubresourceTransitionFlags Flags,
   UINT64 ExecutionId)
{
   // Simultaneous access resources currently in the COMMON
   // state can be implicitly promoted to any state other state.
   // Any non-simultaneous-access resources currently in the
   // COMMON state can still be implicitly  promoted to SRV,
   // NON_PS_SRV, COPY_SRC, or COPY_DEST.
   CCurrentResourceState::LogicalState CurrentLogicalState = CurrentState.GetLogicalSubresourceState(SubresourceIndex);

   // If the last time this logical state was set was in a different
   // execution period and is decayable then decay the current state
   // to COMMON
   if(ExecutionId != CurrentLogicalState.ExecutionId && CurrentLogicalState.MayDecay)
   {
      CurrentLogicalState.State = D3D12_RESOURCE_STATE_COMMON;
      CurrentLogicalState.IsPromotedState = false;
   }
   bool MayDecay = false;
   bool IsPromotion = false;

   bool bQueueStateUpdate = (SubresourceDestinationInfo.Flags & SubresourceTransitionFlags_NotUsedInCommandListIfNoStateChange) == SubresourceTransitionFlags_None;

   // If not promotable then StateIfPromoted will be D3D12_RESOURCE_STATE_COMMON
   auto StateIfPromoted = CurrentState.StateIfPromoted(after, SubresourceIndex, Flags);

   if ( D3D12_RESOURCE_STATE_COMMON == StateIfPromoted )
   {
      if (TransitionRequired(CurrentLogicalState.State, /*inout*/ after, SubresourceDestinationInfo.Flags))
      {
         // Insert a single concrete barrier (for non-simultaneous access resources).
         TransitionDesc.Transition.StateBefore = D3D12_RESOURCE_STATES(CurrentLogicalState.State);
         TransitionDesc.Transition.StateAfter = D3D12_RESOURCE_STATES(after);
         assert(TransitionDesc.Transition.StateBefore != TransitionDesc.Transition.StateAfter);
         m_vResourceBarriers.push_back(TransitionDesc); // throw( bad_alloc )

         MayDecay = CurrentState.SupportsSimultaneousAccess() && !IsD3D12WriteState(after, Flags);
         IsPromotion = false;
         bQueueStateUpdate = true;
      }
   }
   else
   {
      // Handle identity state transition
      if(after != StateIfPromoted)
      {
         after = StateIfPromoted;
         MayDecay = !IsD3D12WriteState(after, Flags);
         IsPromotion = true;
         bQueueStateUpdate = true;
      }
   }

   if (bQueueStateUpdate)
   {
      CCurrentResourceState::LogicalState NewLogicalState{after, ExecutionId, IsPromotion, MayDecay};
      AddCurrentStateUpdate(TransitionableResourceState,
                            CurrentState,
                            TransitionDesc.Transition.Subresource,
                            NewLogicalState);
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::SubmitResourceTransitions(ID3D12GraphicsCommandList *pCommandList)
{
   // Submit any pending barriers on source command lists that are not the destination.
   if (!m_vResourceBarriers.empty())
   {
      pCommandList->ResourceBarrier((UINT)m_vResourceBarriers.size(), m_vResourceBarriers.data());
   }
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::TransitionResource(TransitionableResourceState* pResource, D3D12_RESOURCE_STATES State, SubresourceTransitionFlags Flags)
{
   CDesiredResourceState::SubresourceInfo DesiredState = { State, Flags };
   ResourceStateManager::TransitionResource(*pResource, DesiredState);
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::TransitionSubresource(TransitionableResourceState* pResource, UINT SubresourceIndex, D3D12_RESOURCE_STATES State, SubresourceTransitionFlags Flags)
{
   CDesiredResourceState::SubresourceInfo DesiredState = { State, Flags };
   ResourceStateManager::TransitionSubresource(*pResource, SubresourceIndex, DesiredState);
}

//----------------------------------------------------------------------------------------------------------------------------------
void ResourceStateManager::ApplyAllResourceTransitions(ID3D12GraphicsCommandList *pCommandList, UINT64 ExecutionId, bool bIsPreDraw)
{
   ApplyResourceTransitionsPreamble();

   ForEachTransitioningResource([=](TransitionableResourceState& ResourceBase) -> TransitionResult
   {
       TransitionableResourceState& CurResource = static_cast<TransitionableResourceState&>(ResourceBase);

       ID3D12Resource *pResource = CurResource.GetD3D12Resource();

       return ProcessTransitioningResource(
           pResource,
           CurResource,
           CurResource.GetCurrentState(),
           CurResource.NumSubresources(),
           ExecutionId,
           bIsPreDraw);
   });

   SubmitResourceTransitions(pCommandList);
}
