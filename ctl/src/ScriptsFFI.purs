module ScriptsFFI
  ( rawFUELMintingPolicy
  , rawCommitteeCandidateValidator
  , rawMPTRootTokenMintingPolicy
  , rawMPTRootTokenValidator
  , rawUpdateCommitteeHash
  ) where

foreign import rawFUELMintingPolicy ∷ String
foreign import rawCommitteeCandidateValidator ∷ String
foreign import rawMPTRootTokenMintingPolicy ∷ String
foreign import rawMPTRootTokenValidator ∷ String
foreign import rawUpdateCommitteeHash ∷ String
