{-# LANGUAGE FlexibleInstances #-}

module Cardano.Config.LedgerQueries
  (LedgerQueries(..))
where

import           Prelude (Int, error, (.))

import qualified Data.Map as Map

import           Ouroboros.Consensus.Ledger.Abstract
import           Byron.Spec.Ledger.Core (Relation(..))

import qualified Cardano.Chain.Block as Byron
import qualified Cardano.Chain.UTxO as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Block as Byron
import qualified Ouroboros.Consensus.Byron.Ledger.Ledger as Byron

import qualified Ouroboros.Consensus.Shelley.Ledger as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.UTxO as Shelley

import qualified Ouroboros.Consensus.Cardano as Cardano

import qualified Ouroboros.Consensus.Mock.Ledger as Mock


class LedgerQueries blk where
  ledgerUtxoSize :: LedgerState blk -> Int

instance LedgerQueries Byron.ByronBlock where
  ledgerUtxoSize = size . Byron.unUTxO . Byron.cvsUtxo . Byron.byronLedgerState

instance LedgerQueries (Shelley.ShelleyBlock c) where
  ledgerUtxoSize =
    (\(Shelley.UTxO xs)-> Map.size xs) . Shelley._utxo . Shelley._utxoState . Shelley.esLState . Shelley.nesEs . Shelley.shelleyState

instance LedgerQueries (Cardano.CardanoBlock a) where
  ledgerUtxoSize _ = error "ledgerUtxoSize:  not implemented for CardanoBlock"

instance LedgerQueries (Mock.SimpleBlock a b) where
  ledgerUtxoSize _ = error "ledgerUtxoSize:  not implemented for SimpleBlock"
