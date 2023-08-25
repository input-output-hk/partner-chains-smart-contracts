# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

## Added

* Script versioning system with three new commands: `insert-version`,
  `invalidate-version`, and `update-version`.  First two require `--version`
  followed by a positive integer as an argument.  `update-version` requires
  `--old-version` and `--new-version` options, each followed by a positive
  integer.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* Added `claim-v2` and `burn-v2` commands for claiming and burning fuel using
  version 2 of the scripts.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* `HasField` type class for easier onchain record handling
  ([561](https://github.com/input-output-hk/trustless-sidechain/issues/561),
  [508](https://github.com/mlabs-haskell/trustless-sidechain/pull/508),
  onchain)
* Extra functions for efficient `Data` encoding and decoding for product types
  ([484](https://github.com/mlabs-haskell/trustless-sidechain/issues/484),
  [504](https://github.com/mlabs-haskell/trustless-sidechain/pull/504),
  onchain)
* CLI flags for creating cbor encoded messages and cryptographic primitives has
  been added to the Purescript CLI.
  In particular,  the following CLI commands were added
        ```
        utils key-gen ecdsa-secp256k1
        utils key-gen schnorr-secp256k1
        utils sign ecdsa-secp256k1
        utils sign schnorr-secp256k1
        utils encode cbor-update-committee-message
        utils encode cbor-block-producer-registration-message
        utils encode cbor-merkle-root-insertion-message
        utils encode cbor-merkle-tree-entry
        utils encode cbor-merkle-tree
        utils encode cbor-combined-merkle-proof
        utils encode cbor-plain-aggregate-public-keys
        ```
    ([566](https://github.com/input-output-hk/trustless-sidechain/issues/566)
    , [517](https://github.com/mlabs-haskell/trustless-sidechain/pull/517)
    , offchain)

## Changed

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
* Commands `claim` and `burn` have been renamed to `claim-v1` and `burn-v1`,
  respectively.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))
* Commands `init` and `init-tokens-mint` now require `--version` flag followed
  by a positive integer as an argument.
  ([586](https://github.com/mlabs-haskell/trustless-sidechain/pull/586))

## Fixed

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
  `claim` CLI endpoint is *not* used
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
  preparation for more signature schemes to come) *must* be added to all CLI
  calls to specify which committee certificate verification is being used.
  Alternatively, one can put `"atmsKind": "plain-ecdsa-secp256k1"` in the
  config JSON file.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
   [487]( https://github.com/mlabs-haskell/trustless-sidechain/pull/493),
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
   [487]( https://github.com/mlabs-haskell/trustless-sidechain/pull/493),
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
* `UpdateCommitteeHashMessage` has a new format so committee signatures *must*
  be generated differently for this endpoint. In particular,
  `UpdateCommitteeHashMessage` was changed so that the hash of the
  sorted committee members keys replaces the sorted list of the public keys,
  and a new field which must be an `Address` (see the next bullet point) must
  be included.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
   [487]( https://github.com/mlabs-haskell/trustless-sidechain/pull/487),
   offchain/onchain)
* the subcommand `addresses` for the offchain CLI interface outputs the
  `cbor(plutusData(Address))` of the committee hash validator.
  ([394](https://github.com/mlabs-haskell/trustless-sidechain/issues/394),
   [487]( https://github.com/mlabs-haskell/trustless-sidechain/pull/487),
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
