module ScriptsFFI (fUELMintingPolicy , committeCandidateValidator , mPTRootMintingPolicy , mPTRootTokenMintingPolicy , mPTRootTokenValidator , updateCommitteeHash) where

foreign import fUELMintingPolicy          ∷ String
foreign import committeCandidateValidator ∷ String
foreign import mPTRootMintingPolicy       ∷ String
foreign import mPTRootTokenMintingPolicy  ∷ String
foreign import mPTRootTokenValidator      ∷ String
foreign import updateCommitteeHash        ∷ String
