# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

## Added

* Added script versioning system with three new commands: `insert-version`,
  `invalidate-version`, and `update-version`.  First two require `--version`
  followed by a positive integer as an argument.  `update-version` requires
  `--old-version` and `--new-version` options, each followed by a positive
  integer.

* Added `claim-v2` and `burn-v2` commands for claiming and burning fuel using
  version 2 of the scripts.

* Checks for `--threshold-numerator` and `--threshold-denominator` to ensure
  coprimality
  ([317](https://github.com/mlabs-haskell/trustless-sidechain/issues/317),
   [378](https://github.com/mlabs-haskell/trustless-sidechain/pull/378) offchain).

* Added an optional flag `--distributed-set-utxo` to the `claim` CLI subcommand
  to avoid the linear scan through the UTxO set
  ([412](https://github.com/mlabs-haskell/trustless-sidechain/issues/412),
  [414](https://github.com/mlabs-haskell/trustless-sidechain/pull/414)
  offchain)

* Added a warning when the optional flag `--distributed-set-utxo` in the
  `claim` CLI endpoint is *not* used
  ([412](https://github.com/mlabs-haskell/trustless-sidechain/issues/412),
  [414](https://github.com/mlabs-haskell/trustless-sidechain/pull/414)
  offchain)

## Changed

* Commands `claim` and `burn` have been renamed to `claim-v1` and `burn-v1`,
  respectively.

* `--threshold` is no longer used: `--threshold-numerator` and
  `--threshold-denominator` now behave correctly and are no longer deprecated
  ([317](https://github.com/mlabs-haskell/trustless-sidechain/issues/317),
   [378](https://github.com/mlabs-haskell/trustless-sidechain/pull/378), offchain).
* CTL is now version 5.0.0
  ([395](https://github.com/mlabs-haskell/trustless-sidechain/issues/395),
  offchain).

# v2.0.0

TODO: Backfill

# v1.0.0

First release.
