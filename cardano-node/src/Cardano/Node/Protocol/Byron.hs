{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Node.Protocol.Byron
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolByron

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolByron
    -- * Errors
  , ByronProtocolInstantiationError(..)
  , renderByronProtocolInstantiationError

    -- * Reusable parts
  , readGenesis
  , readLeaderCredentials
  ) where


import           Cardano.Prelude

import           Codec.CBOR.Read (DeserialiseFailure, deserialiseFromBytes)
import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (bimapExceptT, firstExceptT,
                                                   hoistEither, left)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as Text

import qualified Cardano.Chain.Genesis as Genesis
import qualified Cardano.Chain.Update as Update
import qualified Cardano.Chain.UTxO as UTxO
import qualified Cardano.Crypto.Signing as Signing
import           Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus

import           Ouroboros.Consensus.Byron.Ledger (ByronBlock)

import           Cardano.Config.Types
                   (NodeByronProtocolConfiguration (..),
                    ProtocolFilepaths(..), GenesisFile (..))
import           Cardano.TracingOrphanInstances.Byron ()

import           Cardano.Node.Protocol.Types


------------------------------------------------------------------------------
-- Byron protocol
--

-- | Make 'SomeConsensusProtocol' using the Byron instance.
--
-- This lets us handle multiple protocols in a generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolByron
  :: NodeByronProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolByron nc files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolByron fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol <$> mkConsensusProtocolByron nc files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolByron
  :: NodeByronProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ByronProtocolInstantiationError IO
             (Consensus.Protocol IO ByronBlock ProtocolRealPBFT)
mkConsensusProtocolByron NodeByronProtocolConfiguration {
                           npcByronGenesisFile,
                           npcByronReqNetworkMagic,
                           npcByronPbftSignatureThresh,
                           npcByronApplicationName,
                           npcByronApplicationVersion,
                           npcByronSupportedProtocolVersionMajor,
                           npcByronSupportedProtocolVersionMinor,
                           npcByronSupportedProtocolVersionAlt
                         }
                         files = do
    genesisConfig <- readGenesis npcByronGenesisFile npcByronReqNetworkMagic

    optionalLeaderCredentials <- readLeaderCredentials genesisConfig files

    return $
      Consensus.ProtocolRealPBFT
        genesisConfig
        (PBftSignatureThreshold <$> npcByronPbftSignatureThresh)
        (Update.ProtocolVersion npcByronSupportedProtocolVersionMajor
                                npcByronSupportedProtocolVersionMinor
                                npcByronSupportedProtocolVersionAlt)
        (Update.SoftwareVersion npcByronApplicationName
                                npcByronApplicationVersion)
        optionalLeaderCredentials


readGenesis :: GenesisFile
            -> RequiresNetworkMagic
            -> ExceptT ByronProtocolInstantiationError IO
                       Genesis.Config
readGenesis (GenesisFile file) ncReqNetworkMagic =
    firstExceptT (GenesisReadError file) $ do
      (genesisData, genesisHash) <- Genesis.readGenesisData file
      return Genesis.Config {
        Genesis.configGenesisData       = genesisData,
        Genesis.configGenesisHash       = genesisHash,
        Genesis.configReqNetMagic       = ncReqNetworkMagic,
        Genesis.configUTxOConfiguration = UTxO.defaultUTxOConfiguration
        --TODO: add config support for the UTxOConfiguration if needed
      }



readLeaderCredentials :: Genesis.Config
                      -> Maybe ProtocolFilepaths
                      -> ExceptT ByronProtocolInstantiationError IO
                                 (Maybe PBftLeaderCredentials)
readLeaderCredentials _ Nothing = return Nothing
readLeaderCredentials genesisConfig
                      (Just ProtocolFilepaths {
                        byronCertFile,
                        byronKeyFile
                      }) =
  case (byronCertFile, byronKeyFile) of
    (Nothing, Nothing) -> pure Nothing
    (Just _, Nothing) -> left SigningKeyFilepathNotSpecified
    (Nothing, Just _) -> left DelegationCertificateFilepathNotSpecified
    (Just delegCertFile, Just signingKeyFile) -> do

         signingKeyFileBytes <- liftIO $ LB.readFile signingKeyFile
         delegCertFileBytes <- liftIO $ LB.readFile delegCertFile
         signingKey <- firstExceptT (SigningKeyDeserialiseFailure signingKeyFile)
                         . hoistEither
                         $ deserialiseSigningKey signingKeyFileBytes
         delegCert  <- firstExceptT (CanonicalDecodeFailure delegCertFile)
                         . hoistEither
                         $ canonicalDecodePretty delegCertFileBytes

         bimapExceptT PbftError Just
           . hoistEither
           $ mkPBftLeaderCredentials genesisConfig signingKey delegCert

  where
    deserialiseSigningKey :: LB.ByteString
                          -> Either DeserialiseFailure Signing.SigningKey
    deserialiseSigningKey =
        fmap (Signing.SigningKey . snd)
      . deserialiseFromBytes Signing.fromCBORXPrv

------------------------------------------------------------------------------
-- Byron Errors
--

data ByronProtocolInstantiationError =
    CanonicalDecodeFailure !FilePath !Text
  | DelegationCertificateFilepathNotSpecified
  | GenesisConfigurationError !FilePath !Genesis.ConfigurationError
  | GenesisReadError !FilePath !Genesis.GenesisDataError
  | PbftError !PBftLeaderCredentialsError
  | SigningKeyDeserialiseFailure !FilePath !DeserialiseFailure
  | SigningKeyFilepathNotSpecified
  deriving Show


renderByronProtocolInstantiationError :: ByronProtocolInstantiationError -> Text
renderByronProtocolInstantiationError pie =
  case pie of
    CanonicalDecodeFailure fp failure -> "Canonical decode failure in " <> toS fp
                                         <> " Canonical failure: " <> failure
    DelegationCertificateFilepathNotSpecified -> "Delegation certificate filepath not specified"
    --TODO: Implement configuration error render function in cardano-ledger
    GenesisConfigurationError fp genesisConfigError -> "Genesis configuration error in: " <> toS fp
                                                       <> " Error: " <> (Text.pack $ show genesisConfigError)
    GenesisReadError fp err ->  "There was an error parsing the genesis file: " <> toS fp
                                <> " Error: " <> (Text.pack $ show err)
    -- TODO: Implement PBftLeaderCredentialsError render function in ouroboros-network
    PbftError pbftLeaderCredentialsError -> "PBFT leader credentials error: " <> (Text.pack $ show pbftLeaderCredentialsError)
    SigningKeyDeserialiseFailure fp deserialiseFailure -> "Signing key deserialisation error in: " <> toS fp
                                                           <> " Error: " <> (Text.pack $ show deserialiseFailure)
    SigningKeyFilepathNotSpecified -> "Signing key filepath not specified"
