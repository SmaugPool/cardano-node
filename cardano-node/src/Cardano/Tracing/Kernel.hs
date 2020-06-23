{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}

module Cardano.Tracing.Kernel
  ( NodeKernelData (..)
  , mkNodeKernelData
  , setNodeKernel
  , mapNodeKernelDataIO
  , SMaybe (..)
  , fromSMaybe
  -- * Re-exports
  , NodeKernel (..)
  , LocalConnectionId
  , RemoteConnectionId
  ) where

import           Cardano.Prelude hiding (atomically)

import           Data.IORef (IORef, newIORef, readIORef, writeIORef)

import           Ouroboros.Consensus.Node (NodeKernel(..))
import           Ouroboros.Consensus.Util.Orphans ()

import           Ouroboros.Network.NodeToClient (LocalConnectionId)
import           Ouroboros.Network.NodeToNode (RemoteConnectionId)


data SMaybe a
  = SNothing
  | SJust !a
  deriving (Foldable, Functor, Generic, NFData, NoUnexpectedThunks, Traversable)

fromSMaybe :: a -> SMaybe a -> a
fromSMaybe x SNothing = x
fromSMaybe _ (SJust x) = x


newtype NodeKernelData blk =
  NodeKernelData
  { unNodeKernelData :: IORef (SMaybe (NodeKernel IO RemoteConnectionId LocalConnectionId blk))
  }

mkNodeKernelData :: IO (NodeKernelData blk)
mkNodeKernelData = NodeKernelData <$> newIORef SNothing

setNodeKernel :: NodeKernelData blk
              -> NodeKernel IO RemoteConnectionId LocalConnectionId blk
              -> IO ()
setNodeKernel (NodeKernelData ref) nodeKern =
  writeIORef ref $ SJust nodeKern

mapNodeKernelDataIO ::
  (NodeKernel IO RemoteConnectionId LocalConnectionId blk -> IO a)
  -> NodeKernelData blk
  -> IO (SMaybe a)
mapNodeKernelDataIO f (NodeKernelData ref) =
  readIORef ref >>= traverse f
