{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Crypto.Secp256k1 qualified as SECP
import Data.Bits (unsafeShiftL)
import GHC.Exts (fromList)
import Laws (toDataSafeLaws', toDataUnsafeLaws')
import Plutus.V2.Ledger.Api (
  CurrencySymbol,
  LedgerBytes (LedgerBytes),
  ValidatorHash,
 )
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Gen,
  NonNegative (NonNegative),
  Positive (Positive),
  arbitrary,
  chooseBoundedIntegral,
  chooseInteger,
  elements,
  liftArbitrary,
  liftShrink,
  oneof,
  shrink,
  vectorOf,
 )
import Test.QuickCheck.Extra (
  ArbitraryAssetClass (ArbitraryAssetClass),
  ArbitraryBytes (ArbitraryBytes),
  ArbitraryCurrencySymbol (ArbitraryCurrencySymbol),
  ArbitraryPubKeyHash (ArbitraryPubKeyHash),
  ArbitraryTxOutRef (ArbitraryTxOutRef),
  ArbitraryValidatorHash (ArbitraryValidatorHash),
  DA,
 )
import Test.Tasty (adjustOption, defaultMain, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)
import TrustlessSidechain.DistributedSet (
  Ds (Ds),
  DsConfDatum (DsConfDatum),
  DsConfMint (DsConfMint),
  DsDatum (DsDatum),
  DsKeyMint (DsKeyMint),
  Ib (Ib),
  Node (Node),
 )
import TrustlessSidechain.Governance (GovernanceAuthority (GovernanceAuthority))
import TrustlessSidechain.HaskellPrelude
import TrustlessSidechain.MerkleTree (
  MerkleProof (MerkleProof),
  RootHash (RootHash),
  Side (L, R),
  Up (Up),
 )
import TrustlessSidechain.PlutusPrelude qualified as PTPrelude
import TrustlessSidechain.Types (
  ATMSPlainAggregatePubKey (ATMSPlainAggregatePubKey),
  ATMSPlainMultisignature (ATMSPlainMultisignature),
  BlockProducerRegistration (
    BlockProducerRegistration,
    auraKey,
    grandpaKey,
    inputUtxo,
    ownPkh,
    sidechainPubKey,
    sidechainSignature,
    stakeOwnership
  ),
  BlockProducerRegistrationMsg (
    BlockProducerRegistrationMsg,
    inputUtxo,
    sidechainParams,
    sidechainPubKey
  ),
  CandidatePermissionMint (
    CandidatePermissionMint,
    sidechainParams,
    utxo
  ),
  CheckpointParameter (
    CheckpointParameter,
    assetClass,
    committeeCertificateVerificationCurrencySymbol,
    committeeOracleCurrencySymbol,
    sidechainParams
  ),
  CombinedMerkleProof (CombinedMerkleProof),
  CommitteeCertificateMint (
    CommitteeCertificateMint,
    thresholdDenominator,
    thresholdNumerator
  ),
  EcdsaSecp256k1PubKey (EcdsaSecp256k1PubKey),
  MerkleRootInsertionMessage (
    MerkleRootInsertionMessage,
    merkleRoot,
    previousMerkleRoot,
    sidechainParams
  ),
  MerkleTreeEntry (
    MerkleTreeEntry,
    amount,
    index,
    previousMerkleRoot,
    recipient
  ),
  PubKey (PubKey),
  SidechainParams (
    SidechainParams,
    chainId,
    genesisUtxo,
    governanceAuthority,
    thresholdDenominator,
    thresholdNumerator
  ),
  Signature (Signature),
  SignedMerkleRootRedeemer (SignedMerkleRootRedeemer),
  StakeOwnership (AdaBasedStaking, TokenBasedStaking),
  UpdateCommitteeDatum (UpdateCommitteeDatum),
  UpdateCommitteeHash (
    UpdateCommitteeHash,
    committeeCertificateVerificationCurrencySymbol,
    committeeOracleCurrencySymbol,
    mptRootTokenCurrencySymbol,
    sidechainParams
  ),
  UpdateCommitteeHashMessage (
    UpdateCommitteeHashMessage,
    newAggregateCommitteePubKeys,
    previousMerkleRoot,
    sidechainEpoch,
    sidechainParams,
    validatorHash
  ),
  UpdateCommitteeHashRedeemer (UpdateCommitteeHashRedeemer),
 )

main :: IO ()
main =
  defaultMain . adjustOption go . testGroup "Roundtrip" $
    [ testProperty "SidechainParams (safe)" . toDataSafeLaws' genSP shrinkSP $ show
    , testProperty "SidechainParams (unsafe)" . toDataUnsafeLaws' genSP shrinkSP $ show
    , testProperty "EcdsaSecp256k1PubKey" . toDataSafeLaws' genPK shrinkPK $ show
    , testProperty "EcdsaSecp256k1PubKey" . toDataUnsafeLaws' genPK shrinkPK $ show
    , testProperty "CandidatePermissionMint (safe)" . toDataSafeLaws' genCPM shrinkCPM $ show
    , testProperty "CandidatePermissionMint (unsafe)" . toDataUnsafeLaws' genCPM shrinkCPM $ show
    , testProperty "BlockProducerRegistration (safe)" . toDataSafeLaws' genBPR shrinkBPR $ show
    , testProperty "BlockProducerRegistration (unsafe)" . toDataUnsafeLaws' genBPR shrinkBPR $ show
    , testProperty "BlockProducerRegistrationMsg (safe)" . toDataSafeLaws' genBPRM shrinkBPRM $ show
    , testProperty "BlockProducerRegistrationMsg (unsafe)" . toDataUnsafeLaws' genBPRM shrinkBPRM $ show
    , testProperty "MerkleTreeEntry (safe)" . toDataSafeLaws' genMTE shrinkMTE $ show
    , testProperty "MerkleTreeEntry (unsafe)" . toDataUnsafeLaws' genMTE shrinkMTE $ show
    , testProperty "MerkleRootInsertionMessage (safe)" . toDataSafeLaws' genMRIM shrinkMRIM $ show
    , testProperty "MerkleRootInsertionMessage (unsafe)" . toDataUnsafeLaws' genMRIM shrinkMRIM $ show
    , testProperty "SignedMerkleRootRedeemer (safe)" . toDataSafeLaws' genSMRR shrinkSMRR $ show
    , testProperty "SignedMerkleRootRedeemer (unsafe)" . toDataUnsafeLaws' genSMRR shrinkSMRR $ show
    , testProperty "RootHash (safe)" . toDataSafeLaws' genRH shrinkRH $ show
    , testProperty "RootHash (unsafe)" . toDataUnsafeLaws' genRH shrinkRH $ show
    , testProperty "Side (safe)" . toDataSafeLaws' genSide shrinkSide $ show
    , testProperty "Side (unsafe)" . toDataUnsafeLaws' genSide shrinkSide $ show
    , testProperty "Up (safe)" . toDataSafeLaws' genUp shrinkUp $ show
    , testProperty "Up (unsafe)" . toDataUnsafeLaws' genUp shrinkUp $ show
    , testProperty "MerkleProof (safe)" . toDataSafeLaws' genMP shrinkMP $ show
    , testProperty "MerkleProof (unsafe)" . toDataUnsafeLaws' genMP shrinkMP $ show
    , testProperty "CombinedMerkleProof (safe)" . toDataSafeLaws' genCMP shrinkCMP $ show
    , testProperty "CombinedMerkleProof (unsafe)" . toDataUnsafeLaws' genCMP shrinkCMP $ show
    , testProperty "UpdateCommitteeDatum (safe)" . toDataSafeLaws' genUPD shrinkUPD $ show
    , testProperty "UpdateCommitteeDatum (unsafe)" . toDataUnsafeLaws' genUPD shrinkUPD $ show
    , testProperty "ATMSPlainAggregatePubKey (safe)" . toDataSafeLaws' genAPAPK shrinkAPAPK $ show
    , testProperty "ATMSPlainAggregatePubKey (unsafe)" . toDataUnsafeLaws' genAPAPK shrinkAPAPK $ show
    , testProperty "UpdateCommitteeHash (safe)" . toDataSafeLaws' genUCH shrinkUCH $ show
    , testProperty "UpdateCommitteeHash (unsafe)" . toDataUnsafeLaws' genUCH shrinkUCH $ show
    , testProperty "UpdateCommitteeHashMessage (safe)" . toDataSafeLaws' genUCHM shrinkUCHM $ show
    , testProperty "UpdateCommitteeHashMessage (unsafe)" . toDataUnsafeLaws' genUCHM shrinkUCHM $ show
    , testProperty "UpdateCommitteeHashRedeemer (safe)" . toDataSafeLaws' genUCHR shrinkUCHR $ show
    , testProperty "UpdateCommitteeHashRedeemer (unsafe)" . toDataUnsafeLaws' genUCHR shrinkUCHR $ show
    , testProperty "CommitteeCertificateMint (safe)" . toDataSafeLaws' genCCM shrinkCCM $ show
    , testProperty "CommitteeCertificateMint (unsafe)" . toDataUnsafeLaws' genCCM shrinkCCM $ show
    , -- CheckpointDatum needs format clarification
      testProperty "ATMSPlainMultisignature (safe)" . toDataSafeLaws' genAPM shrinkAPM $ show
    , testProperty "ATMSPlainMultisignature (unsafe)" . toDataUnsafeLaws' genAPM shrinkAPM $ show
    , -- CheckpointRedeemer needs format clarification
      testProperty "CheckpointParameter (safe)" . toDataSafeLaws' genCP shrinkCP $ show
    , testProperty "CheckpointParameter (unsafe)" . toDataUnsafeLaws' genCP shrinkCP $ show
    , -- CheckpointMessage needs format clarification
      testProperty "Ds (safe)" . toDataSafeLaws' genDs shrinkDs $ show
    , testProperty "Ds (unsafe)" . toDataUnsafeLaws' genDs shrinkDs $ show
    , testProperty "DsDatum (safe)" . toDataSafeLaws' genDsDatum shrinkDsDatum $ show
    , testProperty "DsDatum (unsafe)" . toDataUnsafeLaws' genDsDatum shrinkDsDatum $ show
    , testProperty "Node (safe)" . toDataSafeLaws' genNode shrinkNode $ show
    , testProperty "Node (unsafe)" . toDataUnsafeLaws' genNode shrinkNode $ show
    , testProperty "DsConfDatum (safe)" . toDataSafeLaws' genDsConfDatum shrinkDsConfDatum $ show
    , testProperty "DsConfDatum (unsafe)" . toDataUnsafeLaws' genDsConfDatum shrinkDsConfDatum $ show
    , testProperty "Ib (safe)" . toDataSafeLaws' genIb shrinkIb $ show
    , testProperty "Ib (unsafe)" . toDataUnsafeLaws' genIb shrinkIb $ show
    , testProperty "DsConfMint (safe)" . toDataSafeLaws' genDsConfMint shrinkDsConfMint $ show
    , testProperty "DsConfMint (unsafe)" . toDataUnsafeLaws' genDsConfMint shrinkDsConfMint $ show
    , testProperty "DsKeyMint (safe)" . toDataSafeLaws' genDsKeyMint shrinkDsKeyMint $ show
    , testProperty "DsKeyMint (unsafe)" . toDataUnsafeLaws' genDsKeyMint shrinkDsKeyMint $ show
    ]
  where
    go :: QuickCheckTests -> QuickCheckTests
    go = max (QuickCheckTests 10_000)

-- Helpers

-- Generators

genNode :: Gen Node
genNode = Node <$> go <*> go
  where
    go :: Gen PTPrelude.BuiltinByteString
    go = do
      ArbitraryBytes (LedgerBytes bbs) <- arbitrary
      pure bbs

genDs :: Gen Ds
genDs =
  Ds <$> do
    ArbitraryCurrencySymbol sym <- arbitrary
    pure sym

genDsDatum :: Gen DsDatum
genDsDatum =
  DsDatum <$> do
    ArbitraryBytes (LedgerBytes bbs) <- arbitrary
    pure bbs

genDsConfDatum :: Gen DsConfDatum
genDsConfDatum = DsConfDatum <$> go <*> go
  where
    go :: Gen CurrencySymbol
    go = do
      ArbitraryCurrencySymbol sym <- arbitrary
      pure sym

genIb :: Gen (Ib DA)
genIb = Ib <$> arbitrary

genDsConfMint :: Gen DsConfMint
genDsConfMint =
  DsConfMint <$> do
    ArbitraryTxOutRef tout <- arbitrary
    pure tout

genDsKeyMint :: Gen DsKeyMint
genDsKeyMint = DsKeyMint <$> go <*> go2
  where
    go :: Gen ValidatorHash
    go = do
      ArbitraryValidatorHash vh <- arbitrary
      pure vh
    go2 :: Gen CurrencySymbol
    go2 = do
      ArbitraryCurrencySymbol sym <- arbitrary
      pure sym

genCP :: Gen CheckpointParameter
genCP = do
  sp <- genSP
  ArbitraryAssetClass ac <- arbitrary
  ArbitraryCurrencySymbol sym <- arbitrary
  ArbitraryCurrencySymbol sym' <- arbitrary
  pure . CheckpointParameter sp ac sym $ sym'

-- Generates arbitrary bytes
genAPM :: Gen ATMSPlainMultisignature
genAPM = ATMSPlainMultisignature <$> go <*> go
  where
    go :: Gen [LedgerBytes]
    go = liftArbitrary $ do
      ArbitraryBytes lb <- arbitrary
      pure lb

genUCHR :: Gen UpdateCommitteeHashRedeemer
genUCHR = UpdateCommitteeHashRedeemer <$> genPMR

genUCHM :: Gen (UpdateCommitteeHashMessage DA)
genUCHM = do
  sp <- genSP
  nacpk <- arbitrary
  pmr <- genPMR
  NonNegative se <- arbitrary
  ArbitraryValidatorHash vh <- arbitrary
  pure . UpdateCommitteeHashMessage sp nacpk pmr se $ vh

genAPAPK :: Gen ATMSPlainAggregatePubKey
genAPAPK =
  ATMSPlainAggregatePubKey <$> do
    ArbitraryBytes lb <- arbitrary
    pure lb

genUPD :: Gen (UpdateCommitteeDatum DA)
genUPD =
  UpdateCommitteeDatum <$> arbitrary <*> do
    NonNegative i <- arbitrary
    pure i

genCMP :: Gen CombinedMerkleProof
genCMP = CombinedMerkleProof <$> genMTE <*> genMP

genMP :: Gen MerkleProof
genMP = MerkleProof <$> liftArbitrary genUp

genUp :: Gen Up
genUp = Up <$> genSide <*> genRH

genSide :: Gen Side
genSide = elements [L, R]

genRH :: Gen RootHash
genRH = RootHash <$> genMR

-- This makes arbitrary bytes for 'recipient': for what we need, it works fine,
-- and it's due to change to Address anyway: https://github.com/mlabs-haskell/trustless-sidechain/blob/e6476e24183e17c87e29b2b1bfa3a53342afedf2/docs/SIPs/09-Generalizing-Token-Transfer-From-Sidechain-to-Mainchain.md?plain=1#L98-L99
genMTE :: Gen MerkleTreeEntry
genMTE = do
  i <- fromIntegral <$> chooseBoundedIntegral (minBound :: Word32, maxBound)
  a <- chooseInteger (0, (1 `unsafeShiftL` 32) - 1)
  ArbitraryBytes r <- arbitrary
  pmr <- genPMR
  pure . MerkleTreeEntry i a r $ pmr

genSMRR :: Gen SignedMerkleRootRedeemer
genSMRR = SignedMerkleRootRedeemer <$> genPMR

genMRIM :: Gen MerkleRootInsertionMessage
genMRIM = do
  sp <- genSP
  mr <- genMR
  pmr <- genPMR
  pure . MerkleRootInsertionMessage sp mr $ pmr

-- | Generates a Merkle root, which is a 256-bit hash.
genMR :: Gen LedgerBytes
genMR =
  LedgerBytes
    . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
    . fromList @ByteString
    <$> vectorOf 32 arbitrary

-- | Generates a previous Merkle root, which could be empty.
genPMR :: Gen (Maybe LedgerBytes)
genPMR = liftArbitrary genMR

genBPRM :: Gen BlockProducerRegistrationMsg
genBPRM = do
  sp <- genSP
  spk <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  ArbitraryTxOutRef tout <- arbitrary
  pure . BlockProducerRegistrationMsg sp spk $ tout

genSO :: Gen StakeOwnership
genSO = oneof [arbitraryAdaBasedCreds, pure TokenBasedStaking]
  where
    arbitraryAdaBasedCreds = do
      ArbitraryPubKey pk <- arbitrary
      ArbitrarySignature sig <- arbitrary
      pure $ AdaBasedStaking pk sig

genBPR :: Gen BlockProducerRegistration
genBPR = do
  so <- genSO
  sidePk <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  auraKey <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  grandpaKey <- (\(EcdsaSecp256k1PubKey pk) -> pk) <$> genPK
  ArbitrarySignature sideSig <- arbitrary
  ArbitraryTxOutRef iu <- arbitrary
  ArbitraryPubKeyHash pkh <- arbitrary
  pure $
    BlockProducerRegistration
      so
      sidePk
      sideSig
      iu
      pkh
      auraKey
      grandpaKey

genPK :: Gen EcdsaSecp256k1PubKey
genPK = do
  seed <- fromList @ByteString <$> vectorOf 32 arbitrary
  case SECP.secKey seed of
    Just privKey ->
      pure
        . EcdsaSecp256k1PubKey
        . LedgerBytes
        . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
        . SECP.exportPubKey True
        . SECP.derivePubKey
        $ privKey
    Nothing -> genPK -- we assume this isn't gonna happen too often

genCCM :: Gen CommitteeCertificateMint
genCCM = do
  Positive tn <- arbitrary
  Positive td <- arbitrary
  pure . CommitteeCertificateMint tn $ td

genUCH :: Gen UpdateCommitteeHash
genUCH = do
  sp <- genSP
  ArbitraryCurrencySymbol cocs <- arbitrary
  ArbitraryCurrencySymbol ccvcs <- arbitrary
  ArbitraryCurrencySymbol rtcs <- arbitrary
  pure . UpdateCommitteeHash sp cocs ccvcs $ rtcs

genGA :: Gen GovernanceAuthority
genGA = do
  ArbitraryPubKeyHash pkh <- arbitrary
  pure $ GovernanceAuthority pkh

genSP :: Gen SidechainParams
genSP = do
  NonNegative cid <- arbitrary
  ArbitraryTxOutRef gu <- arbitrary
  Positive n <- arbitrary
  Positive d <- arbitrary
  ga <- genGA
  pure . SidechainParams cid gu n d $ ga

genCPM :: Gen CandidatePermissionMint
genCPM = do
  sp <- genSP
  ArbitraryTxOutRef x <- arbitrary
  pure . CandidatePermissionMint sp $ x

-- Shrinkers

shrinkDs :: Ds -> [Ds]
shrinkDs (Ds sym) =
  Ds <$> do
    ArbitraryCurrencySymbol sym' <- shrink (ArbitraryCurrencySymbol sym)
    pure sym'

shrinkDsDatum :: DsDatum -> [DsDatum]
shrinkDsDatum (DsDatum bbs) =
  DsDatum <$> do
    ArbitraryBytes (LedgerBytes bbs') <- shrink (ArbitraryBytes (LedgerBytes bbs))
    pure bbs'

shrinkNode :: Node -> [Node]
shrinkNode (Node k n) = do
  ArbitraryBytes (LedgerBytes k') <- shrink (ArbitraryBytes (LedgerBytes k))
  ArbitraryBytes (LedgerBytes n') <- shrink (ArbitraryBytes (LedgerBytes n))
  pure . Node k' $ n'

shrinkDsConfDatum :: DsConfDatum -> [DsConfDatum]
shrinkDsConfDatum (DsConfDatum kp fp) = do
  ArbitraryCurrencySymbol kp' <- shrink (ArbitraryCurrencySymbol kp)
  ArbitraryCurrencySymbol fp' <- shrink (ArbitraryCurrencySymbol fp)
  pure . DsConfDatum kp' $ fp'

shrinkIb :: Ib DA -> [Ib DA]
shrinkIb (Ib x) = Ib <$> shrink x

shrinkDsConfMint :: DsConfMint -> [DsConfMint]
shrinkDsConfMint (DsConfMint tout) =
  DsConfMint <$> do
    ArbitraryTxOutRef tout' <- shrink (ArbitraryTxOutRef tout)
    pure tout'

shrinkDsKeyMint :: DsKeyMint -> [DsKeyMint]
shrinkDsKeyMint (DsKeyMint vh cs) = do
  ArbitraryValidatorHash vh' <- shrink (ArbitraryValidatorHash vh)
  ArbitraryCurrencySymbol cs' <- shrink (ArbitraryCurrencySymbol cs)
  pure . DsKeyMint vh' $ cs'

shrinkCP :: CheckpointParameter -> [CheckpointParameter]
shrinkCP (CheckpointParameter {..}) = do
  sp' <- shrinkSP sidechainParams
  ArbitraryAssetClass ac' <- shrink (ArbitraryAssetClass assetClass)
  ArbitraryCurrencySymbol sym' <- shrink (ArbitraryCurrencySymbol committeeOracleCurrencySymbol)
  ArbitraryCurrencySymbol sym'' <- shrink (ArbitraryCurrencySymbol committeeCertificateVerificationCurrencySymbol)
  pure . CheckpointParameter sp' ac' sym' $ sym''

shrinkAPM ::
  ATMSPlainMultisignature ->
  [ATMSPlainMultisignature]
shrinkAPM (ATMSPlainMultisignature pks sigs) =
  ATMSPlainMultisignature <$> go pks <*> go sigs
  where
    go :: [LedgerBytes] -> [[LedgerBytes]]
    go xs = flip liftShrink xs $ \lb -> do
      ArbitraryBytes lb' <- shrink (ArbitraryBytes lb)
      pure lb'

shrinkUCHR ::
  UpdateCommitteeHashRedeemer ->
  [UpdateCommitteeHashRedeemer]
shrinkUCHR (UpdateCommitteeHashRedeemer pmr) =
  UpdateCommitteeHashRedeemer <$> shrinkPMR pmr

shrinkUCHM ::
  UpdateCommitteeHashMessage DA ->
  [UpdateCommitteeHashMessage DA]
shrinkUCHM (UpdateCommitteeHashMessage {..}) = do
  sp' <- shrinkSP sidechainParams
  nacpk' <- shrink newAggregateCommitteePubKeys
  pmr' <- shrinkPMR previousMerkleRoot
  NonNegative se' <- shrink (NonNegative sidechainEpoch)
  ArbitraryValidatorHash vh' <- shrink (ArbitraryValidatorHash validatorHash)
  pure . UpdateCommitteeHashMessage sp' nacpk' pmr' se' $ vh'

shrinkAPAPK ::
  ATMSPlainAggregatePubKey ->
  [ATMSPlainAggregatePubKey]
shrinkAPAPK (ATMSPlainAggregatePubKey lb) =
  ATMSPlainAggregatePubKey <$> do
    ArbitraryBytes lb' <- shrink (ArbitraryBytes lb)
    pure lb'

shrinkUPD :: UpdateCommitteeDatum DA -> [UpdateCommitteeDatum DA]
shrinkUPD (UpdateCommitteeDatum pks se) =
  UpdateCommitteeDatum <$> shrink pks <*> do
    NonNegative se' <- shrink (NonNegative se)
    pure se'

shrinkCMP :: CombinedMerkleProof -> [CombinedMerkleProof]
shrinkCMP (CombinedMerkleProof t mp) =
  CombinedMerkleProof <$> shrinkMTE t <*> shrinkMP mp

shrinkMP :: MerkleProof -> [MerkleProof]
shrinkMP (MerkleProof ups) = MerkleProof <$> liftShrink shrinkUp ups

shrinkUp :: Up -> [Up]
shrinkUp (Up ss sib) =
  Up <$> shrinkSide ss <*> shrinkRH sib

shrinkSide :: Side -> [Side]
shrinkSide = const []

shrinkRH :: RootHash -> [RootHash]
shrinkRH (RootHash mr) = RootHash <$> shrinkMR mr

shrinkMTE :: MerkleTreeEntry -> [MerkleTreeEntry]
shrinkMTE (MerkleTreeEntry {..}) = do
  i' <- shrink index
  a' <- shrink amount
  ArbitraryBytes r' <- shrink (ArbitraryBytes recipient)
  pmr' <- shrinkPMR previousMerkleRoot
  guard (i' >= 0)
  guard (a' >= 0)
  pure . MerkleTreeEntry i' a' r' $ pmr'

shrinkSMRR :: SignedMerkleRootRedeemer -> [SignedMerkleRootRedeemer]
shrinkSMRR (SignedMerkleRootRedeemer pmr) =
  SignedMerkleRootRedeemer <$> shrinkPMR pmr

shrinkMRIM :: MerkleRootInsertionMessage -> [MerkleRootInsertionMessage]
shrinkMRIM (MerkleRootInsertionMessage {..}) = do
  sp' <- shrinkSP sidechainParams
  mr' <- shrinkMR merkleRoot
  pmr' <- shrinkPMR previousMerkleRoot
  pure . MerkleRootInsertionMessage sp' mr' $ pmr'

-- We don't shrink these, as it wouldn't make much sense to
shrinkMR :: LedgerBytes -> [LedgerBytes]
shrinkMR = const []

shrinkPMR :: Maybe LedgerBytes -> [Maybe LedgerBytes]
shrinkPMR = liftShrink shrinkMR

shrinkBPRM :: BlockProducerRegistrationMsg -> [BlockProducerRegistrationMsg]
shrinkBPRM (BlockProducerRegistrationMsg {..}) = do
  sp' <- shrinkSP sidechainParams
  EcdsaSecp256k1PubKey spk' <- shrinkPK (EcdsaSecp256k1PubKey sidechainPubKey)
  ArbitraryTxOutRef tout' <- shrink (ArbitraryTxOutRef inputUtxo)
  pure . BlockProducerRegistrationMsg sp' spk' $ tout'

shrinkSO :: StakeOwnership -> [StakeOwnership]
shrinkSO = \case
  AdaBasedStaking pk sig -> do
    ArbitraryPubKey pk' <- shrink (ArbitraryPubKey pk)
    ArbitrarySignature sig' <- shrink (ArbitrarySignature sig)
    pure $ AdaBasedStaking pk' sig'
  TokenBasedStaking -> []

shrinkBPR :: BlockProducerRegistration -> [BlockProducerRegistration]
shrinkBPR (BlockProducerRegistration {..}) = do
  so' <- shrinkSO stakeOwnership
  EcdsaSecp256k1PubKey sidePk' <- shrinkPK (EcdsaSecp256k1PubKey sidechainPubKey)
  EcdsaSecp256k1PubKey auraKey' <- shrinkPK (EcdsaSecp256k1PubKey auraKey)
  EcdsaSecp256k1PubKey grandpaKey' <- shrinkPK (EcdsaSecp256k1PubKey grandpaKey)
  ArbitrarySignature sideSig' <- shrink (ArbitrarySignature sidechainSignature)
  ArbitraryTxOutRef tout' <- shrink (ArbitraryTxOutRef inputUtxo)
  ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash ownPkh)
  pure $
    BlockProducerRegistration
      so'
      sidePk'
      sideSig'
      tout'
      pkh'
      auraKey'
      grandpaKey'

-- We don't shrink these, as it wouldn't make much sense to
shrinkPK :: EcdsaSecp256k1PubKey -> [EcdsaSecp256k1PubKey]
shrinkPK = const []

shrinkCCM :: CommitteeCertificateMint -> [CommitteeCertificateMint]
shrinkCCM (CommitteeCertificateMint {..}) = do
  Positive tn' <- shrink (Positive thresholdNumerator)
  Positive td' <- shrink (Positive thresholdDenominator)
  pure . CommitteeCertificateMint tn' $ td'

shrinkUCH :: UpdateCommitteeHash -> [UpdateCommitteeHash]
shrinkUCH (UpdateCommitteeHash {..}) = do
  sp' <- shrinkSP sidechainParams
  ArbitraryCurrencySymbol cocs' <- shrink (ArbitraryCurrencySymbol committeeOracleCurrencySymbol)
  ArbitraryCurrencySymbol csvcs' <- shrink (ArbitraryCurrencySymbol committeeCertificateVerificationCurrencySymbol)
  ArbitraryCurrencySymbol rtcs' <- shrink (ArbitraryCurrencySymbol mptRootTokenCurrencySymbol)
  pure . UpdateCommitteeHash sp' cocs' csvcs' $ rtcs'

shrinkGA :: GovernanceAuthority -> [GovernanceAuthority]
shrinkGA (GovernanceAuthority pkh) = do
  ArbitraryPubKeyHash pkh' <- shrink (ArbitraryPubKeyHash pkh)
  pure $ GovernanceAuthority pkh'

shrinkSP :: SidechainParams -> [SidechainParams]
shrinkSP (SidechainParams {..}) = do
  NonNegative cid' <- shrink . NonNegative $ chainId
  ArbitraryTxOutRef gu' <- shrink . ArbitraryTxOutRef $ genesisUtxo
  Positive n' <- shrink . Positive $ thresholdNumerator
  ga <- shrinkGA governanceAuthority
  -- We don't shrink the denominator, as this could make the result _bigger_.
  pure . SidechainParams cid' gu' n' thresholdDenominator $ ga

shrinkCPM :: CandidatePermissionMint -> [CandidatePermissionMint]
shrinkCPM (CandidatePermissionMint {..}) = do
  sp' <- shrinkSP sidechainParams
  ArbitraryTxOutRef x' <- shrink (ArbitraryTxOutRef utxo)
  pure . CandidatePermissionMint sp' $ x'

-- | Wrapper for 'PubKey' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitraryPubKey = ArbitraryPubKey PubKey
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via PubKey
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as this wouldn't make much sense.
--
-- @since v4.0.0
instance Arbitrary ArbitraryPubKey where
  arbitrary =
    ArbitraryPubKey
      . PubKey
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 64 arbitrary

-- | Wrapper for 'Signature' to provide QuickCheck instances.
--
-- @since v4.0.0
newtype ArbitrarySignature = ArbitrarySignature Signature
  deriving
    ( -- | @since v4.0.0
      Eq
    , -- | @since v4.0.0
      Ord
    , -- | @since v4.0.0
      PTPrelude.Eq
    , -- | @since v4.0.0
      PTPrelude.Ord
    )
    via Signature
  deriving stock
    ( -- | @since v4.0.0
      Show
    )

-- | Does not shrink, as this wouldn't make much sense.
--
-- @since v4.0.0
instance Arbitrary ArbitrarySignature where
  arbitrary =
    ArbitrarySignature
      . Signature
      . LedgerBytes
      . PTPrelude.toBuiltin @_ @PTPrelude.BuiltinByteString
      . fromList @ByteString
      <$> vectorOf 64 arbitrary
