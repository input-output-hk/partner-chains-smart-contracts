# Modular Committee Certificate Verification

## Requirements

- Allow a sidechain at start-time or upgrade-time to change the method of
  verifying a certificate.

- Allow a sidechain to adjust or upgrade cryptographic verifications.

- Provide a simple 'dummy' method as a stand-in for cryptographic features not
  yet available on Cardano smart contracts.

## Overview
This SIP will discuss the background of the implementation of the current
committee certificate verification mechanism, and how we can adjust the system
to support the requirements.

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
To this end, each of these systems are:

- parameterized by the currency symbol of an [NFT
  `CommitteeHashPolicy`](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#61-update-committee-hash)
  (generated from the [`initUtxo` from the initialize sidechain
  transaction](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#1-initialise-contract))
  to allow them to identify the current committee on chain; and
- have hardcoded verification logic to ensure that strictly more than the
  threshold has signed the current committee.

And obviously, before any of these systems may function, one must of course
mint the `CommitteeHashPolicy` NFT which uniquely identifies the current
committee on chain.

This proposal will describe:

1. modularizing the hardcoded logic of committee certificate verification in a
   single minting policy (that scripts can be parameterized by); and

2. demonstrating how this allows one to adjust / upgrade cryptographic
   verifications.

The actual upgrading will be left to the [update
strategy](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/SIPs/01-UpdateStrategy.md),
so the main contribution of this SIP is an argument towards modularity of the
hardcoded the committee certificate verifications.

## Modular Design of Committee Certificate Verification
We first discuss the general structure that the various committee certificate
verification methods will follow.

The key idea is that we will delegate a committee certificate verification to a
minting policy that will mint a token only if the token's name has been
verified by the committee's cryptographic verification mechanisms.
Note that this limits us to provide committee certificate verifications for
messages that are at most 256 bits long (the maximum token name length), but
indeed, any message can be hashed with a cryptographic hash function to be 256
bits, and so the hash can be signed instead.

More precisely, a _committee certificate verification minting policy_ is a
minting policy with the following workflow.

1. The system is initialized by minting an NFT, `CommitteeHashPolicy`, and
   paying the NFT to some validator script which as datum holds a
   representation of the current committee as some abstract type
   `aggregatePubKeys` (indeed, the datum may store more than just the current
   committee such as a sidechain epoch).

2. Then, the committee certificate verification minting policy is parameterized
   by: the currency symbol of the previous `CommitteeHashPolicy`, and some
   fraction `n/d` that denotes the ratio of how many committee members are
   needed to sign a message (more details below).

   The committee certificate verification minting policy mints only if:

    - exactly one token with token name say `tn` of the committee certificate
      verification minting policy is minted[^exactlyOneToken]; and

    - `tn` has strictly more than `size of the current committee * n / d`
      committee members that have verified `tn` from the provided
      multisignature (as a redeemer) of some abstract type `multisignature` where
      the current committee's `aggregatePubKeys` is identified by the NFT
      `CommitteeHashPolicy` (in a reference input).

   Note that the committee certificate verification token is not needed after
   this transaction and may be paid to some arbitrary burn address.

[^exactlyOneToken]: If the restriction of having exactly one token is too
  strong, then this can be generalized to allow any number of tokens.

Later, we will show specific examples of this definition from different
cryptographic primitives. For now, we will take this as a definition and assume
that we have a committee certificate verification policy which follows this
workflow.

We discuss how this can be used within other Plutus scripts that wish to
verify that the current committee has verified a given certificate `M`.
Any such Plutus script must be parameterized by the currency symbol of a
committee certificate verification minting policy and must verify all of the
following:

- the committee certificate verification minting policy has minted a token with
  token name `tn`; and

- `tn` is the cryptographic hash of `M`.

If any such of these Plutus scripts wish to _upgrade_ their cryptographic
verification mechanisms, then this amounts to simply changing which committee
certificate verification minting policy it is parameterized by.
Indeed, this will change the hash of the Plutus script and hence requires an
[update
strategy](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/SIPs/01-UpdateStrategy.md)
to avoid this issue.

## Changes
As mentioned in the [background](#background), there are currently three Plutus
scripts which verify a committee certificate.

In this section, we address each of these scripts and discuss the modifications
for these systems to use a committee certificate verification minting policy.
We will assume that each of these scripts are parameterized by the currency
symbol of some committee certificate verification policy.

### Committee handover mainchain changes
See
[here](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#61-update-committee-hash)
for the original specification.

Like the previous specification, we will have a validator script uniquely
identified by the `CommitteeHashPolicy` which as datum holds the current
committee and sidechain epoch.
We will call this validator `UpdateCommitteeValidator`.

Its datum will be as follows.
```haskell
data UpdateCommitteeDatum aggregatePubKeys = UpdateCommitteeDatum
    { aggregateCommitteePubKeys :: aggregatePubKeys
    , sidechainEpoch :: Integer
    }
```
Note that this is essentially identical to the previous specification with the
only difference being we abstract the representation of the committee's public
keys to some `aggregatePubKeys` type.

As redeemer, `UpdateCommitteeValidator` will take the following
`UpdateCommitteeRedeemer` data type as follows.
```haskell
data UpdateCommitteeMessage aggregatePubKeys = UpdateCommitteeMessage
  { sidechainParams :: SidechainParams
  , newAggregateCommitteePubKeys :: aggregatePubKeys
  , previousMerkleRoot :: Maybe ByteString
    -- ^ last merkle root inserted on chain, unless there is no Merkle root
    -- inserted yet
  , newSidechainEpoch :: Integer
    -- ^ the sidechain epoch of the new committee
  , newValidatorAddress :: Address
    -- ^ the validator address where the new committee will reside (this allows
    -- one to reuse the `CommitteeHashPolicy` when one change the
    -- cryptographic mechanisms)
  }

type UpdateCommitteeRedeemer aggregatePubKeys = UpdateCommitteeMessage aggregatePubKeys
```
where we note that the redeemer is just a wrapper around the message that will
be signed.

Finally, `UpdateCommitteeValidator` succeeds only if the following are all
satisfied:

- The committee certificate verification minting policy mints with token name
  `tn` for which `tn` satisfies `tn == blake2b(cbor(UpdateCommitteeMessage))`
  where `UpdateCommitteeMessage` is as provided in the redeemer

- The NFT `CommitteeHashPolicy` is output at a validator address
  `newValidatorAddress` from the redeemer.
  This validator address must also have as datum `UpdateCommitteeHashDatum` for
  which its `aggregateCommitteePubKeys` is the `newAggregateCommitteePubKeys`
  from the redeemer, and its `sidechainEpoch` is `newSidechainEpoch` from the
  redeemer.

- The `sidechainEpoch` in the current datum is strictly smaller than
  `newSidechainEpoch` as provided in the redeemer (this is to prevent replay
  attacks).

- There is a reference to the `previousMerkleRoot` in the transaction.

In essence, these verifications verify that the this transaction for updating
the committee corresponds to the  signed `UpdateCommitteeMessage` in a
reasonable sense.

Note that this new committee handover mechanism does *not* rely on specifics of
the committee certificate verification mechanism, as it assumes that the
committee certificate verification minting policy will "do the right thing".

### Checkpoint and Merkle Root Insertion (Save Root) changes
These are both essentially identical to as they are given, with the only change
being that instead of doing the hard coded committee certificate verification,
they instead ensure that their signed message matches whatever token the
committee certificate verification minting policy mints.

This requires no changes on the Bridge.

## Discussion of Different Committee Certificate Verification Minting Policies
Since we have finished discussing changes for how the current Plutus scripts
can use a committee certificate verification minting policy, all that remains
is to discuss different ways one may implement a committee certificate
verification minting policy.

The steps all implementations will follow will be

1. Instantiating `UpdateCommitteeHashDatum aggregatePubKeys` with a specific
   type for `aggregatePubKeys`, and choosing a specific type for
   `multisignature` as the redeemer for the committee certificate verification
   minting policy.

2. Defining the committee certificate verification policy.

For an academic reference on these mechanisms, see this
reference[^proofOfStakeSidechains].

[^proofOfStakeSidechains]: Gazi, Peter, et al. "Proof-of-Stake Sidechains." *2019 IEEE Symposium on Security and Privacy (SP)*, IEEE, 2019, pp. 139-56, https://doi.org/10.1109/SP.2019.00040.

### Design of `CommitteePlainATMSPolicy`
The `CommitteePlainATMSPolicy` is the same committee certificate verification
mechanism that is in the current implementation.

We will instantiate the `aggregatePubKeys` type to be the concatenated hash of
public keys
```haskell
-- | Invariant: 'ATMSPlainAggregatePubKey' is the concatenated hash of sidechain
-- public keys. More precisely,
-- @
-- committeePubKeys = sort([key1, key2, ..., keyN])
-- committeePubKeysHash = blake2b(concat(committeePubKeys))
-- keyN - 33 bytes compressed ecdsa public key of a committee member
-- @
type ATMSPlainAggregatePubKey = ByteString
```
and we instantiate the `multisignature` type to two lists of public keys and
their associated signatures
```haskell
-- | Invariant: 'ATMSPlainAggregatePubKey' is sorted lexicographically, and the
-- `atmsSignatures` signatures is a subsequence of the corresponding public
-- keys of `atmsPublicKeys`
data ATMSPlainMultisignature = ATMSPlainMultisignature
    { atmsPublicKeys :: [SidechainPubKey]
    , atmsSignatures :: [ByteString]
    }
```
where we require that the `ATMSPlainAggregatePubKey` is lexicographically
sorted and `[ByteString]` is a subsequence of the corresponding signatures of
`ATMSPlainAggregatePubKey` for a given message.

Then, `CommitteePlainATMSPolicy` takes as redeemer an `ATMSPlainMultisignature`, and
is parameterized by the currency symbol of `CommitteeHashPolicy` and a
threshold `n/d`; and mints only if the following are all satisfied:

- there is a reference input with the `CommitteeHashPolicy` NFT with datum
  `UpdateCommitteeDatum ATMSPlainAggregatePubKey`;

- the concatenated hash of the public keys `atmsPublicKeys` of the redeemer
  `ATMSPlainMultisignature` matches `aggregateCommitteePubKeys` in
  `UpdateCommitteeDatum ATMSPlainAggregatePubKey`; and

- the unique token name of `CommitteePlainATMSPolicy` has been verified by
  strictly more than `length atmsPublicKeys * n / d` public keys and signatures.

Clearly, this satisfies the workflow required for a committee certificate
verification minting policy.

### Design of `CommitteeDummyATMSPolicy`
The `CommitteeDummyATMSPolicy` is a trivial minting policy that verifies
nothing (and always mints) while we wait for new cryptographic primitives to be
added in the blockchain.

We don't discuss this further since it's straightforward and technically does
not satisfy the requirements for a committee certificate verification minting policy

### Design of `CommitteeMultisignatureATMSPolicy`
The `CommitteeMultisignatureATMSPolicy` is an alternative committee certificate
verification mechanism that should be a bit more efficient since it uses proper
cryptographic multisignature schemes, but it unfortunately requires some features
unavailable on Cardano now.

### Required Builtins
We will assume the following builtin functions
```haskell
type GDH = ..

type GDHPubKey = GDH
type GDHSignature = GDH
type MessageDigest = GDH -- cryptographic hash to the GDH group (not including the unit)

gdhMul :: GDH -> GDH -> GDH
gdhDiv :: GDH -> GDH -> GDH
gdhHash :: ByteString -> MessageDigest

ddhVerify :: GDHPubKey -> MessageDigest -> GDHSignature -> Bool

byteStringToGdh :: ByteString -> GDH  -- converts a bytestring to an element of GDH (throwing an error otherwise)
gdhToByteString :: GDH -> ByteString  -- converts a GDH element to a bytestring
```
where

- `GDH` is a type for a *Gap Diffie Hellman Group*;

- `gdhMul` is the group multiplication for `GDH`;

- `gdhDiv` is the inverse of the group multiplication for `GDH`; and

- `ddhVerify` verifies that the provided signature shows that the given public
  key has signed the message digest.

For details, see these references[^shortSignaturesFromTheWeilPairing][^thresholdSignaturesMultisignaturesAndBlindSignaturesBasedOnTheGDHGroupSignatureScheme].

[^shortSignaturesFromTheWeilPairing]: Boneh, Dan, et al. "Short Signatures from the Weil Pairing." *Journal of Cryptology*, vol. 17, no. 4, 2004, pp. 297-319, https://doi.org/10.1007/s00145-004-0314-9.

[^thresholdSignaturesMultisignaturesAndBlindSignaturesBasedOnTheGDHGroupSignatureScheme]: Boldyreva, Alexandra. "Threshold Signatures, Multisignatures and Blind Signatures Based on the Gap-Diffie-Hellman-Group Signature Scheme." *Public Key Cryptography - PKC 2003*, Springer Berlin Heidelberg, 2003, pp. 31-46, https://doi.org/10.1007/3-540-36288-6_3.

Now, we will recall some simple facts about the GDH group (without proof).

- Given public keys `key1`, ... `keyN` of the GDH group, we can create an
  *aggregate public key* by simply multiplying each of the keys together i.e.,
  ```haskell
  aggregateKeys = key1 `gdhMul` ... `gdhMul` keyN
  ```
  and similarly, given signatures (of the aforementioned keys)
  `sig1`, ... `sigN`, we can create an multisignature by simply multiplying the
  signatures together
  ```haskell
  multisignature = sig1 `gdhMul` ... `gdhMul` sigN
  ```

  Then, rather surprisingly, the original `ddhVerify` will verify that the
  provided multisignature shows that the provided aggregated public key (and
  hence *all* the public keys `key1`,..,`keyN`) has signed a message digest --
  subject to rogue key attacks[^rogueKeyAttacks].

[^rogueKeyAttacks]: Ristenpart, Thomas, and Scott Yilek. "The Power of Proofs-of-Possession: Securing Multiparty Signatures Against Rogue-Key Attacks." *Advances in Cryptology - EUROCRYPT 2007*, Springer Berlin Heidelberg, pp. 228–45, https://doi.org/10.1007/978-3-540-72540-4_13.

### Implementation
Assuming we have the aforementioned builtin functions, we are almost ready to
discuss how to implement `CommitteeMultisignatureATMSPolicy`.
But first, note that we will make use of functionality and types from [Merkle
trees implmemented
here](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/onchain/src/TrustlessSidechain/MerkleTree.hs).

Then, to implement `CommitteeMultisignatureATMSPolicy`, we will instantiate the
`aggregatePubKeys` type with the product of each of the committee member's
public keys, the number of members in the committee, and the root of a merkle tree of
all of the committee members
```haskell
data ATMSMultisignatureAggregatePubKey =
    ATMSMultisignatureAggregatePubKey
    { atmsAggregatePubKeys :: GDHPubKey
        -- ^ the product (in a GDH group) of all of the committee
        -- member's public keys
    , atmsCommitteeeSize :: Integer
        -- ^ the number of members in the committee
    , atmsMerkleRoot :: RootHash
        -- ^ a merkle root of all of the current committee's public keys
    }
```
and we instantiate the `multisignature` type with the signature of the GDH
group, the public keys of committee members who did *not* sign the message, and
merkle proofs of the committee members who did not sign the message in
`atmsMerkleRoot`
```haskell
data ATMSMultisignatureSignature = ATMSMultisignatureSignature
    { atmsSignature :: GDHSignature
        -- ^ the product (in a GDH group) of all the committee members who
        -- signed the message
    , atmsNonSigningPubKeys :: [GDHPubKey]
        -- ^ the committee members who did not sign the message
    , atmsNonSigningPubKeysMerkleProofs :: [MerkleProof]
        -- ^ the merkle proofs for the corresponding committee members' who
        -- did not sign the message sorted lexicographically
    }
```
where we require that `atmsNonSigningPubKeysMerkleProofs` are the
corresponding distinct merkle proofs of the given `atmsNonSigningPubKeys` and
sorted lexicographically (to allow testing for distinctness of merkle proofs
easily -- later we will see why this is desired).

Then, `CommitteeMultisignatureATMSPolicy` takes as redeemer an `ATMSMultisignatureSignature`, and
is parameterized by the currency symbol of `CommitteeHashPolicy` and a
threshold `n/d`; and mints only if the following are satisfied.

- There is a reference input with the `CommitteeHashPolicy` NFT with datum
  `UpdateCommitteeDatum ATMSMultisignatureAggregatePubKey`.

- `length atmsNonSigningPubKeys == length atmsNonSigningPubKeysMerkleProofs` is true,
  and write this quantity as `numNonSigners` (the number of nonsigners). We must further verify that
  `atmsCommitteeeSize - numNonSigners > atmsCommitteeeSize * n / d` i.e., there
  are strictly more signers than the required threshold.

- Every `atmsNonSigningPubKeysMerkleProofs` is distinct (requiring that these
  are sorted lexicographically makes this check easy) and are all in the
  `atmsMerkleRoot` (i.e., this verifies that every non signer is actually in
  the current committee).

- The unique token name `tn` minted for `ATMSMultisignatureSignature` satisfies
    ```haskell
    ddhVerify
        -- the public key of the committee except for the non signers
        (atmsAggregatePubKeys `gdhDiv` (atmsNonSigningPubKey1 `gdhMul` .. `gdhMul` atmsNonSigningPubKeyN))
        -- the message we wish to verify is signed
        (byteStringToGdh tn)
        -- the multisignature
        atmsSignature
    ```
    i.e., the public key of the committee except for the non signers (the non
    signers are divided out of the aggregated public key) have signed the
    message.

### Design of `CommitteePoKATMSPolicy`
The `CommitteePoKATMSPolicy` is an alternative committee certificate
verification mechanism that uses proofs of knowledge for the committee
certificate verification mechanism.

TODO

## Conclusion
We have discussed a design for modularizing the committee certificate verification mechanism.
Fundamentally, this proposes to modularize the "committee signing part" into a
single common minting policy that mints only if the current committee has
signed its minted token name.
We demonstrated how current Plutus scripts can be adapted to fit in this
framework, and finally discussed some ways one may implement a committee
certificate verification mechanism.
