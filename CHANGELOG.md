# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

## Changed

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

## Fixed

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
