# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

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
