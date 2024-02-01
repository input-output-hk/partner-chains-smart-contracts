# Integration Guideline (v5)

This document was written for Sidechain bridge developers, who want to integrate
with the `trustless-sidechain` toolkit.

For a general understanding of the Cardano smart contract concepts, architecture
and workflows, it's advised to read the [Architecture] document
first.

## 1. Public API

In contrast to Solidity or similar systems, there is no deployed script to
interact with. As such, defining a public API is a bit more complicated.
In our case we define our public API as a combination of:
- commands of the off-chain CLI tool
- some on-chain data (directly observed data)

In order to be able to observe data on-chain, the bridge has to know the
validator addresses or currency symbols of these scripts. These can be obtained
by the the `nix run .#sidechain-main-cli -- addresses | jq` command.

> Note: these addresses can change with a protocol update (see [SIP-01])
> Observing the VersionOracle is a more stable way of following the addresses of the current version.

For details about the usage of the CLI commands, refer to the [CLI documentation][CLI-doc].

Some action don't require any interaction from the Bridge, but can optionally wrap the `sidechain-main-cli`
for ease of use.

In the following sections we will go through all interactions of
`trustless-sidechain`, and their associated CLI commands and observed data types.

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
| Event                                             | CLI command                                                               | Observed address/token                                            | Observed/Generated data type  |
|---------------------------------------------------|---------------------------------------------------------------------------|-------------------------------------------------------------------|-------------------------------|
| Initialisation                                    | `init` / `init-tokens-mint`                                        | -                                                                 | -                             |
| Insert/Update/Remove D-parameter                  | `insert-d-parameter` / `update-d-parameter` / `remove-d-parameter` | [DParameterToken], [DParameterValidator]                          | [DParameterValidatorDatum]    |
| Insert/Update/Remove permissioned candidate list  | `update-permissioned-candidates`                                   | [PermissionedCandidatesPolicy], [PermissionedCandidatesValidator] | [PermissionedCandidateKeys]   |
| Candidate permission token mint                   | `candidate-permission-token`                                       | [CandidatePermissionToken]                                        | -                             |
| Committee registration/deregistration             | `register` / `deregister`                                          | [CommitteeCandidateValidator]                                     | [BlockProducerRegistration]   |
| Merkle root insertion                             | `save-root`                                                        | [MerkleRootTokenValidator], [MerkleRootTokenMintingPolicy]        | -                             | <!-- TODO: is it observed? -->
| Save checkpoint                                   | `save-checkpoint`                                                  | -                                                                 | -                             | <!-- TODO: is it observed? -->
| Committee hash update                             | `committee-hash`                                                   | -                                                                 | -                             |
| Insert/Update/Invalidate version                  | `insert-version` / `update-version` / `invalidate-version`         | [VersionOraclePolicy], [VersionOracleValidator]                   | [VersionOracle]               |
| Claim tokens                                      | `claim-v1` / `claim-v2`                                            | -                                                                 | [CombinedMerkleProof]         |
| Burn tokens                                       | `burn-v1` / `burn-v2`                                              | -                                                                 | -                             |

### 1.1. Initialisation

No action from the Bridge is required.

### 1.2 Committee registration

Individual SPOs can directly use the CLI to submit a registration.

Bridge must observe [CommitteeCandidateValidator] for new UTxO with a datum.
Datum definition: [BlockProducerRegistration]
Datum encoding: [BlockProducerRegistration-example] -> [BlockProducerRegistration-encoding]

Bridge must also observe removals of the above UTxOs and handle them as deregistration requests.

### 1.3 Merkle root insertion

After the Bridge collects the cross-chain transactions, it has to build a Merkle tree
and submit the Merkle root to the chain. The format of the Merkle tree is described below
is explained in detail in the [Merkle Tree module].

The leaf nodes of the Merkle tree are CBOR encoded [MerkleTreeEntries][MerkleTreeEntry] blake2b hashed.
Definition: [MerkleTreeEntry]
Encoding: [MerkleTreeEntry-example] -> [MerkleTreeEntry-encoding]

### 1.3 Committee hash update

For a valid committee hash update, the committee members must sign the CBOR encoded
[UpdateCommitteeHashMessage].

Definition: [UpdateCommitteeHashMessage]
Encoding: [MerkleTreeEntry-example] -> [UpdateCommitteeHashMessage-encoding]

### 1.4 Token claim

In order to claim the tokens from sidechain to mainchain, a CBOR encoded [CombinedMerkleProof] must be
submitted.

Definition: [CombinedMerkleProof]
Encoding: [CombinedMerkleProof-example] -> [CombinedMerkleProof-encoding]

[Architecture]: ./Architecture.md
[CLI-doc]: ../offchain/README.md
[SIP-01]: ./SIPs/01-UpdateStrategy.md

[Merkle Tree module]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/MerkleTree.hs

[CommitteeCandidateValidator]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/CommitteeCandidateValidator.hs#L28
[MerkleRootTokenValidator]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/MerkleRootTokenValidator.hs#L15
[MerkleRootTokenMintingPolicy]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/MerkleRootTokenMintingPolicy.hs#L77
[CandidatePermissionToken]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/CandidatePermissionMintingPolicy.hs#L26
[VersionOraclePolicy]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Versioning.hs#L242
[VersionOracleValidator]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/Versioning.hs#L375

[DParameterToken]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/DParameter.hs#L46
[DParameterValidator]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/DParameter.hs#L117
[DParameterValidatorDatum]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/Types.hs#L801

[PermissionedCandidatesPolicy]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/PermissionedCandidates.hs#L57
[PermissionedCandidatesValidator]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/PermissionedCandidates.hs#L139
[PermissionedCandidateKeys]: https://github.com/input-output-hk/trustless-sidechain/blob/a0d1204ac21c88b4436b457f24bd856fa947e74e/onchain/src/TrustlessSidechain/Types.hs#L911

[BlockProducerRegistration]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Types.hs#L269
[BlockProducerRegistration-example]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Types.hs#L269
[BlockProducerRegistration-encoding]: https://github.com/input-output-hk/trustless-sidechain/blob/szg251/integration-guideline/onchain/test/golden/BlockProducerRegistration1-isdata.golden

[MerkleTreeEntry]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Types.hs#L387
[MerkleTreeEntry-example]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/test/Test/TrustlessSidechain/Types.hs#L264
[MerkleTreeEntry-encoding]: https://github.com/input-output-hk/trustless-sidechain/blob/szg251/integration-guideline/onchain/test/golden/MerkleTreeEntry-isdata.golden

[UpdateCommitteeHashMessage]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Types.hs#L732
[UpdateCommitteeHashMessage-example]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/test/Test/TrustlessSidechain/Types.hs#L322
[UpdateCommitteeHashMessage-encoding]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/test/Test/TrustlessSidechain/Types.hs#L322

[CombinedMerkleProof]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Types.hs#L532
[CombinedMerkleProof-example]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/test/Test/TrustlessSidechain/Types.hs#L287
[CombinedMerkleProof-encoding]: https://github.com/input-output-hk/trustless-sidechain/blob/szg251/integration-guideline/onchain/test/golden/CombinedMerkleProof-isdata.golden

[VersionOracle]: https://github.com/input-output-hk/trustless-sidechain/blob/31e551802be62385b212428040184c45e68cd572/onchain/src/TrustlessSidechain/Versioning.hs#L131
