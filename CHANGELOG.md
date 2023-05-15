# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).

# Unreleased

## Added

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
