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
    `FUEL` to be transferred between sidechain and mainchain is insufficient as
    later SIPs required designing tokens which fundamentally serve a different
    purpose than `FUEL` and hence require different verifications.
Examples of such tokens from other SIPs include:

- Arbitrary Cardano native asset token transfer.

- Bridging arbitrary data.

Thus, this document proposes that instead of *only* allowing `FUEL` to be
    transferred between sidechain and mainchain, we instead allow an
    arbitrary collection of tokens to be transferred between sidechain and
    mainchain.

From the mainchain's point of view, all transactions from the sidechain *must*
be signed in some sense by the sidechain's committee.
In particular, we recall from the white paper that:

1. Sidechain stakeholders bundle up transactions in a Merkle tree, and
   collectively sign its Merkle root.

2. A signed Merkle root contains the transactions certified by the sidechain,
   and hence these transactions may occur on the mainchain.

Following this line of reasoning, it's clear that we would like to isolate the
    functionality which proves that a transaction is in a signed Merkle root.
Indeed, this is precisely condition 1. in the `FUEL` token that we are
    interested in modularising out.

Early revisions of this document suggested having a separate token for
    modularising the functionality to prove a transaction is in a Merkle root,
    but this idea was ultimately discarded due to the possibility of incurring
    extra ada costs in transactions.
Thus, the modularization in this document is purely "conceptual", and we aim to
    document how one may write a token which may be bridged between sidechain
    and mainchain.

As an overview, this document will discuss the following.

1. We will recall `FUELMintingPolicy`, and rename this to `ScToken` which will
   be essentially identical to `FUELMintingPolicy` with some minor
   improvements.

2. With the ideas in mind from this document, we will sketch how the mainchain
   contracts can be extended to allow bridging of other tokens from sidechain
   to mainchain.
   Specifically, we will sketch:

    - Transferring native Cardano assets from the sidechain to the mainchain
      (assuming that such assets are locked up in the mainchain already).

    - Bridging arbitrary data from the sidechain to the mainchain

## Replacing `FUELMintingPolicy` with `ScToken`

We will start with defining `MerkleTreeEntry` as follows

```haskell
data MerkleTreeEntry
    = ScTokenMerkleTreeEntry
        { amount :: Integer
            -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
        , recipient :: Address
            -- the address of a recipient
        , tokenName :: TokenName
            -- 32 byte token name (often "FUEL")
        , previousMerkleRoot :: Maybe ByteString
            -- previousMerkleRoot is added to make sure that the hashed entry
            -- is unique w.r.t other Merkle roots to prevent double claiming
        }
```

Note that this is essentially identical to the original `MerkleTreeEntry`
```haskell
data MerkleTreeEntry = MerkleTreeEntry
  { index :: Integer -- 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
  , amount :: Integer -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
  , recipient :: ByteString -- arbitrary length bytestring that represents decoded bech32 cardano address
  , previousMerkleRoot :: Maybe ByteString -- previousMerkleRoot is added to make sure that the hashed entry is unique
  }
```
except that we instead:

- added a `tokenName` field to generalize the token name of tokens transferred
  from the sidechain;

- changed the type of `recipient` to match the representation of recipients
  onchain;

- removed the `index` field which is no longer needed to ensure uniqueness
  amongst transactions within the Merkle tree (we instead propose to use the
  Merkle proof).

As redeemer, `ScToken` will take the following data type.
```haskell
data ScTokenRedeemer
    = MainToSide
        ByteString -- recipient sidechain address
    | SideToMain
        MerkleTreeEntry
        MerkleProof
```

Then, `ScToken` mints only if all of the following are satisfied.

- The redeemer is `SideToMain merkleTreeEntry merkleProof`, and
  `merkleTreeEntry` is `ScTokenMerkleTreeEntry`.
- There is a `MerkleRootTokenMintingPolicy` provided as reference input at a
  `MerkleRootTokenValidator` address with Merkle root `merkleRoot`.
- `merkleProof` shows that `cbor(merkleTreeEntry)` is in `merkleRoot`.
- This transaction corresponds to `merkleTreeEntry` in the sense that
  the `recipient` has at least `amount` `ScToken` with `tokenName`, and exactly
  `amount` `ScToken` is minted with `tokenName`.
- `blake2b(cbor(merkleProof,previousMerkleRoot))` is not included in the
  distributed set, and a new entry
  `blake2b(cbor(merkleProof,previousMerkleRoot))` is inserted in the
  distributed set.
  This is to ensure that participants cannot "double mint" their tokens.

  Note that `merkleProof`s are unique in a Merkle tree, and the
  `previousMerkleRoot` ensures uniqueness amongst different Merkle trees.
  Thus, every transaction `ScTokenMerkleTreeEntry` is unique w.r.t its Merkle
  proof and the previous Merkle root.

`ScToken` burns only if all of the following are satisfied.

- the redeemer is `MainToSide`.

### Offchain Requirements for `ScToken`
Similarly to `FUELMintingPolicy`, offchain code should do the following.

- _Mainchain to sidechain transfer._ Offchain code should observe transactions
  which burn `ScToken` noting the amount burned and the sidechain recipient in
  the redeemer. With this information, the sidechain node should mint the
  corresponding amount in the sidechain.

- _Sidechain to mainchain transfer._ At certain points in time, sidechain nodes
  bundle up all sidechain transactions which transfer tokens to the mainchain
  by constructing `ScTokenMerkleTreeEntry`s, say `scme1,...,scmeN`, and create
  a Merkle tree by computing
```
mt = merkleTree([cbor(scme1),...,cbor(scmeN)])
```
  and submitting the Merkle root (after collecting the required signatures) of
  `mt` with `MerkleRootTokenMintingPolicy`.

## Other Tokens
This section discusses how to extend the mainchain contracts to allow
sidechains to transfer different tokens between sidechain and mainchain.

All of the following examples will follow the same few steps, so we sketch out
the steps, then discuss some concrete examples of other tokens to be
transferred between mainchain and sidechain.

Suppose we would like to transfer a token `GreatDAppIdeaToken` between sidechain
and mainchain.
This amounts to completing the following:

1. Modify the `MerkleTreeEntry` type to include another "arm" which includes
   the data, say `GreatDAppIdeaMerkleTreeEntry { .. }`, required by
   `GreatDAppIdeaToken`. In other words, we must have
```diff
data MerkleTreeEntry
    = ScTokenMerkleTreeEntry
    ...
+   | GreatDAppIdeaToken { .. }
    ...
```

2. Implement `GreatDAppIdeaToken` to verify all of the following:

    - a provided Merkle proof shows that the `GreatDAppIdeaMerkleTreeEntry`
      is in a Merkle root from `MerkleRootTokenMintingPolicy`; and

    - whatever other great ideas from `GreatDAppIdeaToken` that need to be
      verified.

### Arbitrary Cardano Native Asset Token Transfer.
Details will be in [this SIP](./docs/SIPs/07-ModularisingTokenHandling.md).

The use case is as follows.

1. There exists some token on Cardano (e.g. tokens representing concert
   tickets) which are completely unrelated to a sidechain.

2. If a user would like to bridge such tokens from the mainchain to the
   sidechain, the user would pay such tokens to some address, call such an
   address a `LockBox`, which is observed by the sidechain to mint the
   corresponding token on the sidechain.

3. If a user would like to bridge such tokens from the sidechain to the
   mainchain, the user would issue a sidechain transaction which would transfer
   it to the mainchain, and be eventually bundled up in a Merkle root. When the
   user claims their transaction from the Merkle root, this amounts to
   unlocking previously locked tokens at some number of `LockBox`s.

To implement this, we first need a `LockBox` address to lock tokens.
Hence, there will need to be a `LockBoxValidator` address which succeeds only if
a token, `ReleaseToken` (described later), mints.

Now, we detail the key parts of the mainchain contracts which allow controlled
claiming of Cardano assets from the sidechain to the mainchain.

We first extend `MerkleTreeEntry` to include an extra "arm" which
contains enough information to identify a Cardano assset.
```diff
data MerkleTreeEntry
    = ...
+   | LockBoxMerkleTreeEntry
+       { amount :: Integer
+           -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
+       , recipient :: Address
+           -- the address of a recipient
+       , lockedCurrencySymbol :: CurrencySymbol
+           -- currency symbol of the token to unlock on the mainchain
+       , lockedTokenName :: TokenName
+           -- token name of the token to unlock on the mainchain
+       , previousMerkleRoot :: Maybe ByteString
+           -- previousMerkleRoot is added to make sure that the hashed entry
+           -- is unique w.r.t other Merkle roots to prevent double claiming
+       }
```

Note that the new entry `LockBoxMerkleTreeEntry` contains fields to identify a
certain amount of a Cardano asset, and the `previousMerkleRoot` is necessary to
ensure that these transactions are unique for the distributed set.

Then, we will create a token, say `ReleaseToken`, which will take as redeemer a
Merkle proof, say `merkleProof`, which shows that the cbor of a
`LockBoxMerkleTreeEntry`, say `lockBoxMerkleTreeEntry`, is in a Merkle root.
Using the `lockBoxMerkleTreeEntry`, this token will mint only if the following
are all satisfied.

- There is a `MerkleRootTokenMintingPolicy` provided as reference input at a
  `MerkleRootTokenValidator` address with Merkle root `merkleRoot`, and
  `merkleProof` shows that `cbor(lockBoxMerkleTreeEntry)` is in `merkleRoot`.

- The recipient receives at least `amount` of the given Cardano asset that is
  `lockedCurrencySymbol` with `lockedTokenName` (locked currently in
  `LockBoxValidator`);

- The remaining Cardano assets in the transaction input `LockBoxValidator` are
  transferred back to `LockBoxValidator` transaction outputs.

- Using that this `LockBoxMerkleTreeEntry` is unique w.r.t its `merkleProof`
  and `previousMerkleRoot`, we must check that `blake2b(cbor(merkleProof,
  previousMerkleRoot))` is not in the distributed set and is inserted in the
  distributed set in this transaction (similar to `ScToken`).
  This is to prevent double claiming.

### Bridging Arbitrary Data
Details will be in another SIP.

The use case is as follows.

1. The sidechain wants to bridge some data to the mainchain so that sidechains
   can can observe / react to this data at some specified address.
   Moreover, it should be easy for sidechains to attest to the validity of the
   data transferred from another sidechain by e.g. testing if the specified
   address has a particular token.

To this end, we will need a data type, `PostBoxValidatorDatum`, which will
    include either the hash of arbitrary data or the data to be transferred
    from a sidechain, and some extra auxillary information about the data.
Moreover, we will also need a token `PostBoxToken` which will attest to the
    validity of the data `PostBoxValidatorDatum` which was sent from sidechain
    to mainchain.

The design of `PostBoxToken` will follow the same steps outlined above.
We first need to add an "arm" to `MerkleTreeEntry` as follows.
```diff
data MerkleTreeEntry
    = ...
+  | PostBoxMerkleTreeEntry
+      { targetAddress :: Address
+          -- the address to send the data to
+      , postBoxData :: PostBoxValidatorDatum
+          -- the data to put at @targetAddress@.
+      -- , previousMerkleRoot :: Maybe ByteString
+      --     -- (optional) previousMerkleRoot to ensure that the data can be
+      --     -- transferred from sidechain to mainchain at most once
+      }
```
Note that `PostBoxMerkleTreeEntry` contains enough information to send some
    data (`postBoxData`) to some address (`targetAddress)`.
Also, if we require that the data can be transferred from sidechain to
    mainchain at most once, then we would need to include `previousMerkleRoot` and
    verify that this transaction is being inserted in the distributed set for the
    first time.

Then, `PostBoxToken` will take as redeemer a Merkle proof, say `merkleProof`, which
    shows that the cbor of a `PostBoxMerkleTreeEntry`, say
    `postBoxMerkleTreeEntry`, is in a Merkle root.
Using the `postBoxMerkleTreeEntry`, `PostBoxToken` will mint only if the
    following are all satisfied.

- There is a `MerkleRootTokenMintingPolicy` provided as reference input at a
  `MerkleRootTokenValidator` address with Merkle root `merkleRoot`, and
  `merkleProof` shows that `cbor(postBoxMerkleTreeEntry)` is in `merkleRoot`.

- There exists a transaction output `targetAddress` with `postBoxData` as
  datum and `PostBoxToken` is paid to this address.

- Exactly one `PostBoxToken` is minted.

- (optional) If we require uniqueness of transferring data from sidechain to
  mainchain, we would verify that
  `blake2b(cbor(merkleProof,previousMerkleRoot))` is inserted in the
  distributed set in this transaction.
