# Generalizing Token Transfer from Sidechain to Mainchain

## Introduction
This document outlines a method to generalize the tokens transferred from
    sidechain to mainchain.
Currently, the only token that the sidechain may transfer to the mainchain is
    the `FUEL` token which wraps a token on the sidechain and verifies all of
    the following conditions.

1. `FUEL` has been certified by the sidechain committee (by verifying that this
   transaction is in a signed Merkle root).

2. `FUEL` is not double minted.

3. `FUEL` is received by the intended recipient.

In later SIPs, it became clear that this limitation of only allowing
    `FUEL` to be transferred is insufficient as later SIPs required designing
    tokens which fundamentally serve a different purpose than `FUEL` and hence
    must verify different conditions.
Examples of such tokens from other SIPs include:

    - Arbitrary Cardano native asset token transfer.

    - Bridging arbitrary data.

    - Updating oracle feeds of data.

Thus, this document proposes that instead of *only* allowing `FUEL` to be
    transferred from the sidechain to the mainchain, we instead allow an
    arbitrary collection of tokens to be transferred from the sidechain to the
    mainchain.

From the mainchain's point of view, all transactions from the sidechain *must*
be signed in some sense by the sidechain's committee.
In particular, we recall that (TODO: cite the white paper)

1. Sidechain stakeholders bundle up transactions in a Merkle tree, and
   collectively sign its Merkle root.

2. A signed Merkle root contains the transactions certified by the sidechain,
   and hence these transactions may occur on the mainchain.

Following this line of reasoning, it's clear that we would like to isolate the
    functionality which proves that a transaction is in a signed Merkle root.
Indeed, this is precisely condition 1. in the `FUEL` token that we are
    interested in modularising out.

The rest of this document will be as follows.

1. We will give the specification for a token `CertifiedMerkleTreeEntryMintingPolicy` which
   mints only if a `MerkleTreeEntry` (transaction) is in a signed Merkle root.

2. We will show how `CertifiedMerkleTreeEntryMintingPolicy` can be used implement `FUEL`

3. We will sketch how `CertifiedMerkleTreeEntryMintingPolicy` can be used to allow the
   sidechain to transfer other tokens to the mainchain which implement
   functionality required by other SIPs such as

    - Transferring native Cardano assets from the sidechain to the mainchain
      (assuming that such assets are locked up in the mainchain already).

    - Bridging arbitrary data from the sidechain to the mainchain

    - Updating oracle feeds of data

4. We will discuss tradeoffs of this approach.

## `CertifiedMerkleTreeEntryMintingPolicy` Plutus Specification
As discussed previously, `CertifiedMerkleTreeEntryMintingPolicy` isolates the
functionality of verifying a transaction is in a signed Merkle root.
In this section, we precisely describe its functionality.

We will need a data type `MerkleTreeEntry` (as in the original specification)
which represents the transactions from the sidechain.
```haskell
data MerkleTreeEntry
```
For now, we will not include any constructors but the intuition is that this
contains enough information to represent transactions from the sidechain.

Recall from the main specification (TODO: link this) that the
`SignedMerkleRoot` policy mints only if its token name is a signed Merkle root
of `MerkleTreeEntry`s from the sidechain.

`CertifiedMerkleTreeEntryMintingPolicy` will take as redeemer a
`CertifiedMerkleTreeEntryRedeemer` defined as follows.
```haskell
data CertifiedMerkleTreeEntryRedeemer
    CertifiedMerkleTreeEntryRedeemer
        { cborMerkleTreeEntry :: BuiltinByteString
        , merkleProof :: MerkleProof
        }
```
[TODO: link the merkle proof]
The idea is that this `CertifiedMerkleTreeEntryRedeemer` contains enough
information to prove that given a `mte :: MerkleTreeEntry`, then `cbor(mte)`
can be proven to be in a Merkle root with the proof `merkleProof`.

Finally, `CertifiedMerkleTreeEntryMintingPolicy` must parameterized by the
currency symbol of `SignedMerkleRoot` and mints only if all of the following
are satisfied:

- There exists a transaction input which has a unique `SignedMerkleRoot` token
  with token name `merkleRoot`.

- `cborMerkleTreeEntry` and `merkleProof` from its redeemer show that
  `cborMerkleTreeEntry` is in `merkleRoot`

- The unique token name of `CertifiedMerkleTreeEntryMintingPolicy` is
  `blake2b_256(cborMerkleTreeEntry)`.

To summarize, `CertifiedMerkleTreeEntryMintingPolicy` verifies that the cbor of
a `MerkleTreeEntry` is in a signed Merkle root, and the unique token name
of`CertifiedMerkleTreeEntryMintingPolicy` must be `blake2b_256(cborMerkleTreeEntry)`.
The latter condition allows other Plutus scripts to verify that a
`MerkleTreeEntry` has been certified by the sidechain by testing if such a
`MerkleTreeEntry`'s cbor hash is minted with `CertifiedMerkleTreeEntryMintingPolicy`.

## Revamped `FUEL` Plutus Specification

## Sketching Other Tokens.

## Tradeoffs
