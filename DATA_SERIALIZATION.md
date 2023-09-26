= `Data` serialization notes

We specify the method of serialization for each onchain type, both in Haskell
and Purescript. Additionally, we specify whether the serialization is allowed to
change or not.

| __Type__ | __Haskell serialization__ | __Purescript serialization__ | __Can change__ |
| -- | -- | -- | -- |
| `BlockProducerRegistration` | TH-derived | Product | **No** |
| `BlockProducerRegistrationMsg` | TH derived | Not provided | No |
| `CandidatePermissionMint` | Product | Product | Yes |
| `CheckpointDatum` | Product | Product | Yes |
| `CheckpointMessage` | TH derived | Mimicked | No |
| `CheckpointParameter` | Product | Product | Yes |
| `CheckpointRedeemer` | Product | Product | Yes |
| `CombinedMerkleProof` | TH derived | Mimicked | No |
| `Ds` | `newtype` derived | `newtype` derived | Yes |
| `DsConfDatum` | Product | Product | Yes |
| `DsConfMint` | `newtype` derived | `newtype` derived | Yes |
| `DsDatum` | `newtype` derived | `newtype` derived | Yes |
| `DsKeyMint` | Product | Product | Yes |
| `FUELMint` | Product | Product | Yes |
| `FUELRedeemer` | TH derived | Mimicked | Yes |
| `GenesisHash` | `newtype` derived | Doesn't exist | Yes |
| `Ib` | Manual | Not provided | Yes |
| `MerkleProof` | `newtype` derived | `newtype` derived | No |
| `MerkleRootInsertionMessage` | TH derived | Mimicked | No |
| `MerkleTree` | TH derived | Mimicked | Yes |
| `MerkleTreeEntry` | TH derived | Mimicked | No |
| `Node` | Product | Not provided | Yes |
| `RootHash` | `newtype` derived | `newtype` derived | Yes |
| `Side` | Manual | Mimicked | Yes |
| `SidechainParams` | TH derived | Mimicked | No |
| `SidechainPubKey` | `newtype` derived | `newtype` derived | No |
| `SignedMerkleRoot` | Product | Product | Yes |
| `Up` | TH derived | Mimicked | No |
| `UpdateCommitteeHash` | Product | Product | Yes |
| `UpdateCommitteeHashDatum` | Product | Product | Yes |
| `UpdateCommitteeHashMessage` | TH derived | Mimicked | No |
| `UpdateCommitteeHashRedeemer` | Product | Product | Yes |

The definitions for the table entries are as follows:

* __Product__: Defined using `product*Data*` functions.
* __TH derived__: Defined using `makeIsDataIndexed` via TH.
* __newtype derived__: Defined via a `newtype` derivation from the underlying
  type.
* __Not provided__: No `Data` serialization exists.
* __Mimicked__: Defined by mimicking the strategy used by `makeIsDataIndexed`.
* __Doesn't exist__: No such type.
