module ScriptsFFI
  ( fUELMintingPolicy
  , committeeCandidateValidator
  , mPTRootTokenMintingPolicy
  , mPTRootTokenValidator
  , updateCommitteeHash
  ) where

foreign import fUELMintingPolicy ∷ String
foreign import committeeCandidateValidator ∷ String
foreign import mPTRootTokenMintingPolicy ∷ String
foreign import mPTRootTokenValidator ∷ String
foreign import updateCommitteeHash ∷ String
