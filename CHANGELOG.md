# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

## Added

  * Added `use-candidate-permission-token` switch to `register` and `addresses`
    command.  Using this flag will assume usage of candidate permission tokens.
  * Added CLI endpoint `list-versioned-scripts` switch to return currently versioned scripts.
  * Added CLI endpoint `init-token-status` to return all unspent init tokens
    associated with the user's wallet.
  * `getSidechainAddresses` now also returns the `InitTokenPolicy`. That
    affects both the `init` and `addresses` CLI endpoints.
  * Effect system over the `Contract` monad from the CTL library, plus a mechanism to intercept and
    re-interpret errors from the `Contract` monad.
    https://github.com/input-output-hk/trustless-sidechain/pull/723
  * Added CLI versioning and commit hash to sidechain-main-cli with the
    subcommand `cli-version`.
  * Added CLI endpoint `init-tokens-mint` to mint all init tokens by
    spending the genesis UTxO.
  * Added CLI endpoint `init-committee-selection` to initialise the
    committee selection mechanism.
  * Added CLI endpoint `init-checkpoint` to insert scripts and initialise
    checkpointing.
  * Added CLI endpoint `init-fuel` to initialize the FUEL mechanism,
    namely the distributed set and FUEL minting / burning versioning.
  * Added CLI endpoint `init-merkle-root` to initialize the Merkle Root
    policy and validator scripts.

## Changed
  * `init` now prints more human friendly message, when selected genesis utxo
    is not available at user's wallet address.
    ([714](https://github.com/input-output-hk/trustless-sidechain/pull/714))
  * `VersionOracleValidator` now doesn't allow updating utxos anymore. If you
    want to update your scripts, you have to insert new versions and remove
    old version in separate transactions. `update-version` does this under the
    hood.
  * `init` now returns a field `initTransactionIds` instead of
    `versioningTransactionIds`.  This fields includes all initialization
    transactions, not just the ones for the versioning system.
  * NFT token name for Checkpoint and Committee Oracle set to empty string.
    Init token name for Committee Oracle set to `Committee oracle InitToken`.
    Previous values were invalid due to using wrong parsing function.
  * Some `String` types changed to strict `ByteString` where appropriate.
  * `insert-version` is now called `insert-version-2`. It only updates
    a protocol from version 1 to version 2 and only attempts to update the
    version for features already existing in version 1.

## Removed

  * Removed `init-tokens-mint` command.
  * Removed `use-init-tokens` flag from `init` command.
  * Removed `candidate-permission-token-utxo` and
    `candidate-permission-token-name` optional parameters from `register` and
    `candidate-permission-token` commands.
  * Removed `candidate-permission-token-utxo` optional parameter from
    `addresses` command.
  * `remove-d-parameter` command has been removed.
    ([679](https://github.com/input-output-hk/trustless-sidechain/pull/679))

# v5.0.0

## Added
* Added `update-permissioned-candidates` command takes candidates to add and candidates to remove.
  It can also remove all current permissioned-candidates with `--remove-all-candidates` flag.
  ([631](https://github.com/input-output-hk/trustless-sidechain/pull/631))
* Added `collect-garbage` command for burning nfts that can pile up at the user's
  wallet address.
* Script versioning system with three new commands: `insert-version`,
  `invalidate-version`, and `update-version`. First two require `--version`
  followed by a positive integer as an argument. `update-version` requires
  `--old-version` and `--new-version` options, each followed by a positive
  integer.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* Added `claim-v2` and `burn-v2` commands for claiming and burning fuel using
  version 2 of the scripts.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* D Parameter storage and management system, with `insert-d-parameter`,
  `update-d-parameter` and `remove-d-parameter` commands
  ([607](https://github.com/input-output-hk/trustless-sidechain/pull/607))

## Changed
* `update-permissioned-candidates` doesn't take `mainchainKey` as argument anymore.
  For example instead of `--add-candidate MAINCHAIN_KEY:SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY`
  now you have to do `--add-candidate SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY`.
* The `--sidechain-genesis-hash` now only needs to be passed to `--init` and
  `init-tokens-mint` commands.
* `BlockProducerRegistration` has 2 new fields - `authorityDiscoveryKey` and `grandpaKey`
  ([627](https://github.com/input-output-hk/trustless-sidechain/pull/627))
* `register` now doesn't take `--sidechain-pub-key SIDECHAIN_KEY` argument.
  Instead of that it takes `--sidechain-public-keys SIDECHAIN_KEY:AURA_KEY:GRANDPA_KEY` argument.
  ([627](https://github.com/input-output-hk/trustless-sidechain/pull/627))
* `addresses` doesn't return cbor encoded address of UpdateCommitteeHashValidator anymore. Now it returns its validator hash.
  ([614](https://github.com/input-output-hk/trustless-sidechain/pull/614))
* `addresses` no loner requires `init` to be conducted first.  This reverts to
  earlier behaviour that was changed when the versioning system was implemented.
  ([616](https://github.com/input-output-hk/trustless-sidechain/pull/616))
* `addresses` no longer returns `CommitteeNftPolicy` and `CommitteeHashPolicy`
  in the `mintingPolicies` list.  These were duplicates of the
  `CommitteeOraclePolicy` minting policy.
  ([616](https://github.com/input-output-hk/trustless-sidechain/pull/616))
* commands `committee-handover` and `committee-hash` now require `new-committee-validator-hash` argument instead of `new-committee-validator-cbor-encoded-address`.
  ([614](https://github.com/input-output-hk/trustless-sidechain/pull/614))
* Commands `claim` and `burn` have been renamed to `claim-v1` and `burn-v1`,
  respectively.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* Commands `init` and `init-tokens-mint` now require `--version` flag followed
  by a positive integer as an argument.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* On-chain scripts now return short error codes instead of lengthy error
  messages.

## Removed

* `genesisHash` field has been removed from the `"sidechainParameters"` record
  in the configuration file.

# v4.0.0

## Added

* `HasField` type class for easier onchain record handling
  ([561](https://github.com/input-output-hk/trustless-sidechain/issues/561),
  [508](https://github.com/mlabs-haskell/trustless-sidechain/pull/508),
  onchain)
* Extra functions for efficient `Data` encoding and decoding for product types
  ([484](https://github.com/mlabs-haskell/trustless-sidechain/issues/484),
  [504](https://github.com/mlabs-haskell/trustless-sidechain/pull/504),
  onchain)
* `Eq` and `Show` instances for `UpdateCommitteeHash`, `SidechainParams`,
* `ToData` instance for `Node`
  ([569](https://github.com/input-output-hk/trustless-sidechain/issues/569),
  [532](https://github.com/mlabs-haskell/trustless-sidechain/pull/532),
  offchain)
* `FromData` instances for `SidechainParams`, `MerkleRootInsertionMessage`,
  `FUELMint`, `UpdateCommitteeHash`, `UpdateCommitteeHashMessage`,
  `CommitteeCertificateMint`, `CheckpointParameter`, `Node`,
  `BlockProducerRegistrationMsg`,
  `SignedMerkleRootRedeemer`, `SignedMerkleRootMint` and `CandidatePermissionMint`
  ([569](https://github.com/input-output-hk/trustless-sidechain/issues/569),
  [532](https://github.com/mlabs-haskell/trustless-sidechain/pull/532),
  offchain)
* `Eq` instances for `SidechainParams`, `MerkleTreeEntry`, `DsConfMint`,
  `MerkleRootInsertionMessage`, `SignedMerkleRootRedeemer`, `DsConfDatum`,
  `SignedMerkleRootMint`, `CombinedMerkleProof`, `FUELMint`, `Ds`, `DsDatum`,
  `UpdateCommitteeDatum`, `UpdateCommitteeHash`, `UpdateCommitteeHashMessage`,
  `DsKeyMint`, `Node`, `BlockProducerRegistrationMsg`,
  `UpdateCommitteeRedeemer`, `CommitteeCertificateMint`, `CheckpointParameter`,
  `BlockProducerRegistration` and `CandidatePermissionMint`
  ([569](https://github.com/input-output-hk/trustless-sidechain/issues/569),
  [532](https://github.com/mlabs-haskell/trustless-sidechain/pull/532),
  offchain)
* `Show` instances for `BlockProducerRegistration`, `Ds`, `DsDatum`,
  `DsConfMint`, `DsKeyMint`, `Node`, `BlockProducerRegistrationMsg`,
  `MerkleRootInsertionMessage`, `SignedMerkleRootRedeemer`,
  `SignedMerkleRootMint`, `FUELMint`, `UpdateCommitteeDatum`,
  `CommitteeCertificateMint`, `DsConfDatum`,
  `UpdateCommitteeHash`, `UpdateCommitteeHashMessage`, `UpdateCommitteeRedeemer`
  and `CandidatePermissionMint`
  ([569](https://github.com/input-output-hk/trustless-sidechain/issues/569),
  [532](https://github.com/mlabs-haskell/trustless-sidechain/pull/532),
  offchain)
* `Generic` instances for `UpdateCommitteeDatum` and
  `BlockProducerRegistrationMsg`
  ([569](https://github.com/input-output-hk/trustless-sidechain/issues/569),
  [532](https://github.com/mlabs-haskell/trustless-sidechain/pull/532),
  offchain)
* `Eq` and `Show` instances for `UpdateCommitteeHash`, `SidechainParams`,
  `CandidatePermissionMint`, `CommitteeCertificateMint`,
  `BlockProducerRegistrationMsg`, `MerkleRootInsertionMessage`,
  `SignedMerkleRootRedeemer`, `MerkleTreeEntry`, `CombinedMerkleProof`,
  `FUELRedeemer`, `UpdateCommitteeDatum`, `ATMSPlainAggregatePubKey`,
  `UpdateCommitteeHashMessage`, `UpdateCommitteeHashRedeemer`,
  `ATMSPlainMultisignature`, `CheckpointParameter`, `DsConfDatum`,
  `DsConfMint`,
  `BlockProducerRegistration` and `FUELMint`.
* Roundtrip tests for `Data` encodings of onchain types
  ([568](https://github.com/input-output-hk/trustless-sidechain/issues/568),
  [522](https://github.com/mlabs-haskell/trustless-sidechain/pull/522),
* If the `addresses` subcommand has `--atms-kind plain-ecdsa-secp256k1`, then
  the returned JSON object outputs also includes
  `addresses.mintingPolicies.CommitteePlainEcdsaSecp256k1ATMSPolicyId`.
  Otherwise, if the `addresses` subcommand has `--atms-kind plain-schnorr-secp256k1`, then
  the returned JSON object outputs also includes
  `addresses.mintingPolicies.CommitteePlainSchnorrSecp256k1ATMSPolicyId`.
  ([578](https://github.com/input-output-hk/trustless-sidechain/issues/578),
  [524](https://github.com/mlabs-haskell/trustless-sidechain/pull/524),
  onchain)
* CLI flags for creating cbor encoded messages and cryptographic primitives has
  been added to the Purescript CLI.
  In particular, the following CLI commands were added
  `utils key-gen ecdsa-secp256k1
utils key-gen schnorr-secp256k1
utils sign ecdsa-secp256k1
utils sign schnorr-secp256k1
utils encode cbor-update-committee-message
utils encode cbor-block-producer-registration-message
utils encode cbor-merkle-root-insertion-message
utils encode cbor-merkle-tree-entry
utils encode cbor-merkle-tree
utils encode cbor-combined-merkle-proof
utils encode cbor-plain-aggregate-public-keys`
  ([566](https://github.com/input-output-hk/trustless-sidechain/issues/566)
  , [517](https://github.com/mlabs-haskell/trustless-sidechain/pull/517)
  , offchain)

## Changed

* Efficient versions of `Data` encoding functions for product types used where
* Removed `plutus-apps` dependencies
  ([540](https://github.com/mlabs-haskell/trustless-sidechain/pull/540), onchain)
* Modified `BlockProducerRegistration` data type according to SIP-11
  ([43](https://github.com/input-output-hk/trustless-sidechain/issues/43),
  [533](https://github.com/mlabs-haskell/trustless-sidechain/pull/533), onchain and offchain)
* Added CLI arguments `--ada-based-staking` and `--native-token-based-staking` to `register`
  and `deregister` commands, and changed `--spo-public-key` and `--spo-signature` to be only
  required for Ada based staking model
  ([43](https://github.com/input-output-hk/trustless-sidechain/issues/43),
  [533](https://github.com/mlabs-haskell/trustless-sidechain/pull/533), onchain and offchain)
* Efficient versions of `Data` encoding functions for product types used where
  possible ([484](https://github.com/mlabs-haskell/trustless-sidechain/issues/484),
  [504](https://github.com/mlabs-haskell/trustless-sidechain/pull/504),
  onchain)
* `SidechainPublicKey` renamed to `EcdsaSecp256k1PubKey`. Functions around it
  have also been similarly renamed: for example, `sidechainPublicKey` is now
  `ecdsaSecp256k1PubKey`
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain and onchain)
* `SidechainSignature` renamed to `EcdsaSecp256k1Signature`. Functions around it
  have also been similarly renamed: for example, `sidechainSignature` is now
  `ecdsaSecp256k1Signature`
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `SidechainMessage` renamed to `EcdsaSecp256k1Message`. Functions around it
  have also been similarly renamed: for example, `sidechainMessage` is now
  `ecdsaSecp256k1Message`
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `SidechainPrivateKey` renamed to `EcdsaSecp256k1PrivateKey`. Functions around
  it have also been similarly renamed: for example, `sidechainPrivateKey` is now
  `ecdsaSecp256k1PrivateKey`
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `TrustlessSidechain.HaskellPrelude` now exports `Coercible` and `coerce`.

## Fixed

* Distributed set is no longer vulnerable to token dusting in
  `mkInsertValidator`
  ([38](https://github.com/input-output-hk/trustless-sidechain/38),
  [537](https://github.com/mlabs-haskell/trustless-sidechain/pull/537),
  onchain)
* Types whose `Data` encodings cannot change have been noted as such (
  [484](https://github.com/mlabs-haskell/trustless-sidechain/issues/484),
  [504](https://github.com/mlabs-haskell/trustless-sidechain/pull/504),
  onchain)
* `FromData` for `EcdsaSecp256k1PubKey` (formerly `SidechainPublicKey`) now
  correctly checks its length invariant
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `FromData` for `EcdsaSecp256k1Signature` (formerly `SidechainSignature`) now
  correctly checks its length invariant
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/526),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `FromData` for `EcdsaSecp256k1Message` (formerly `SidechainMessage`) now
  correctly checks its length invariant
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/562),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)
* `FromData` for `EcdsaSecp256k1PrivateKey` (formerly `SidechainPrivateKey`) now
  correctly checks its invariants
  ([562](https://github.com/input-output-hk/trustless-sidechain/issues/526),
  [511](https://github.com/mlabs-haskell/trustless-sidechain/pull/511),
  offchain)

# v3.0.0

## Added

* Added checkpoint validator address and checkpoint currency symbol to `addresses` command
  ([506](https://github.com/mlabs-haskell/trustless-sidechain/pull/506) offchain)
* Improved error handling with an application-wide error type
  ([471](https://github.com/mlabs-haskell/trustless-sidechain/issues/471),
  [492](https://github.com/mlabs-haskell/trustless-sidechain/pull/492) offchain)
* Checks for `--threshold-numerator` and `--threshold-denominator` to ensure
  coprimality
  ([317](https://github.com/mlabs-haskell/trustless-sidechain/issues/317),
  [378](https://github.com/mlabs-haskell/trustless-sidechain/pull/378) offchain)
* Optional flag `--distributed-set-utxo` to the `claim` CLI subcommand
  to avoid the linear scan through the UTxO set
  ([412](https://github.com/mlabs-haskell/trustless-sidechain/issues/412),
  [414](https://github.com/mlabs-haskell/trustless-sidechain/pull/414)
  offchain)
* Warning when the optional flag `--distributed-set-utxo` in the
  `claim` CLI endpoint is _not_ used
  ([412](https://github.com/mlabs-haskell/trustless-sidechain/issues/412),
  [414](https://github.com/mlabs-haskell/trustless-sidechain/pull/414)
  offchain)
* New sublibrary `quickcheck-extra` with some QuickCheck helpers
  ([435](https://github.com/mlabs-haskell/trustless-sidechain/issues/435),
  [444](https://github.com/mlabs-haskell/trustless-sidechain/pull/444),
  onchain)
* QuickCheck guide
  ([449](https://github.com/mlabs-haskell/trustless-sidechain/issues/449),
  [468](https://github.com/mlabs-haskell/trustless-sidechain/pull/468),
  onchain)
* `toEncoding` for `SidechainCommitteeMember`
* Size measurements for exported scripts
  ([426](https://github.com/mlabs-haskell/trustless-sidechain/issues/426),
  [473](https://github.com/mlabs-haskell/trustless-sidechain/pull/473), onchain)
* `abs` for absolute value in Haskell prelude, with the same usage caveats as
  `signum`.
* The flag `--atms-kind` with a value such as `plain-ecdsa-secp256k1` (in
  preparation for more signature schemes to come) _must_ be added to all CLI
  calls to specify which committee certificate verification is being used.
  Alternatively, one can put `"atmsKind": "plain-ecdsa-secp256k1"` in the
  config JSON file.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
  [487](https://github.com/mlabs-haskell/trustless-sidechain/pull/493),
  offchain)
  ([83](https://github.com/input-output-hk/trustless-sidechain/issues/83),
  [501](https://github.com/mlabs-haskell/trustless-sidechain/pull/501)
  offchain/onchain)
* The flag `--atms-kind` may have value `plain-schnorr-secp256k1` which allows
  sidechain public keys and signatures to be Schnorr secp256k1 public keys and
  Schnorr secp256k1 signatures.
  ([83](https://github.com/input-output-hk/trustless-sidechain/issues/83),
  [503](https://github.com/input-output-hk/trustless-sidechain/issues/562)
  offchain/onchain)
* The flag `--new-committee-validator-cbor-encoded-address` or `--new-committee-validator-bech32-address`
  was added to the `committee-hash` endpoint for the offchain CLI interface
  which takes either hex encoded cbor of an address of a validator script or a
  bech32 address of a validator script for which the committee oracle should be
  sent to.
  In the former case, this desired address can be found in the `addresses` CLI
  endpoint under the JSON keys `cborEncodedAddresses.CommitteeHashValidator`.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
  [487](https://github.com/mlabs-haskell/trustless-sidechain/pull/493),
  offchain)

## Changed

* `--threshold` is no longer used: `--threshold-numerator` and
  `--threshold-denominator` now behave correctly and are no longer deprecated
  ([317](https://github.com/mlabs-haskell/trustless-sidechain/issues/317),
  [378](https://github.com/mlabs-haskell/trustless-sidechain/pull/378), offchain)
* CTL is now version 5.0.0 (removing ogmios-datum-cache from runtime dependencies)
  ([395](https://github.com/mlabs-haskell/trustless-sidechain/issues/395),
  offchain)
* Refactored configuration file parser
  ([460](https://github.com/mlabs-haskell/trustless-sidechain/issues/460),
* New Haskell standards document and compliance
  ([441](https://github.com/mlabs-haskell/trustless-sidechain/issues/441),
  [450](https://github.com/mlabs-haskell/trustless-sidechain/pull/450),
  onchain)
* `UpdateCommitteeHashMessage` has a new format so committee signatures _must_
  be generated differently for this endpoint. In particular,
  `UpdateCommitteeHashMessage` was changed so that the hash of the
  sorted committee members keys replaces the sorted list of the public keys,
  and a new field which must be an `Address` (see the next bullet point) must
  be included.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
  [487](https://github.com/mlabs-haskell/trustless-sidechain/pull/487),
  offchain/onchain)
* the subcommand `addresses` for the offchain CLI interface outputs the
  `cbor(plutusData(Address))` of the committee hash validator.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
  [487](https://github.com/mlabs-haskell/trustless-sidechain/pull/487),
  offchain)

## Fixed

* Deregistration fail after multiple registrations with the same SPO public key
  ([236](https://github.com/mlabs-haskell/trustless-sidechain/issues/236)

## Removed

* Aeson-related instances for onchain types (not needed).
* `Generic` derivations for those types where they were only used for
  Aeson-related instances.

# v2.0.0

## Added

* Added a standalone benchmark tool
  ([333](https://github.com/mlabs-haskell/trustless-sidechain/pull/333),
  [347](https://github.com/mlabs-haskell/trustless-sidechain/pull/347))
* Introduced Sidechain Improvement Proposals
  ([377](https://github.com/mlabs-haskell/trustless-sidechain/pull/377))
* Added Permissioned committee candidate flow
  ([355](https://github.com/mlabs-haskell/trustless-sidechain/pull/355),
  [364](https://github.com/mlabs-haskell/trustless-sidechain/pull/364))
* Added Checkpointing
  ([373](https://github.com/mlabs-haskell/trustless-sidechain/pull/373))

## Changed

* Refactored and renamed internal modules
  ([330](https://github.com/mlabs-haskell/trustless-sidechain/pull/330),
  [344](https://github.com/mlabs-haskell/trustless-sidechain/pull/344),
  [348](https://github.com/mlabs-haskell/trustless-sidechain/pull/348),
  [349](https://github.com/mlabs-haskell/trustless-sidechain/pull/349),
  [353](https://github.com/mlabs-haskell/trustless-sidechain/pull/353))
* Renamed CLI tool executable to `sidechain-main-cli`
  ([344](https://github.com/mlabs-haskell/trustless-sidechain/pull/344))
* CTL in now v4.0.2 (adding Kupo and removing ctl-server from runtime dependencies)
  ([346](https://github.com/mlabs-haskell/trustless-sidechain/pull/346))
* Security enchancement for Merkle root insertion
  ([361](https://github.com/mlabs-haskell/trustless-sidechain/pull/361))

# v1.0.0

First release.
