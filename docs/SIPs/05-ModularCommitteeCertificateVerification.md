# Modular Committee Certificate Verification

## Requirements

- Allow a sidechain at start-time or upgrade-time to change the method of
  verifying a certificate.

- Allow a sidechain to adjust or upgrade cryptographic verifications.

- Provide a simple 'dummy' method as a stand-in for cryptographic features not
  yet available on Cardano smart contracts.

## Overview
This SIP will discuss the background of the implementation of the current
committee certificate verification, and how we can adjust the system to support
the requirements.

## Background

In the current implementation, the committee verifies certificates in three
places:

1. [Committee handover](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#6-committee-handover)

2. [Transfer FUEL tokens from sidehcain to mainchain](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#3-transfer-fuel-tokens-from-sidechain-to-mainchain)

3. [Checkpointing](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#7-checkpointing)

In all three cases, the same code for the committee certificate verifications
is hardcoded inside each of those systems i.e., each of those systems have
hardcoded logic to verify that enough of the current committee onchain has
signed a message.
More precisely, each of these systems are:

- parameterized by the currency symbol of an NFT (generated from the
  [`initUtxo` from the initialize sidechain
  transaction](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#1-initialise-contract))
  to allow them to identify the current committee on chain; and
- have hardcoded verification logic to ensure that strictly more than the
  threshold has signed the current committee.

Thus, this proposal will describe:

1. modularizing the hardcoded logic of committee certificate verification in a
   single minting policy (that scripts can be parameterized by); and

2. demonstrating how this allows one to adjust / upgrade cryptographic
   verifications.

## Design
Since
