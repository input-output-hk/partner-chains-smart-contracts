# Modularising token handling

## Requirements
The end goal is to be able to deploy two types of sidechains:

1. fuel tokens are owned by the sidechain (lock/unlock), a wrapped token exists
   on the mainchain (mint/burn) (current flow)

2. fuel tokens are owned by the mainchain, a wrapped token exists on the
   sidechain

## Background
The current semantics of the onchain Plutus code to provide the
mechanism for which an asset is transferred between the mainchain and the
sidechain (vice versa) is realized with the `FUEL` token which accomplishes the
following.

- A transfer from *mainchain* to *sidechain* burns a participant's `FUEL`
  tokens and specifying the sidechain recipient (as redeemer). So, for the
  sidechain recipient to receive their corresponding sidechain tokens in the
  sidechain, the Bridge must observe that a mainchain transaction has burnt
  `FUEL`, and unlock the corresponding sidechain tokens to the recipient on the
  sidechain.

- A transfer from *sidechain* to *mainchain* amounts to creating a Merkle root
  of such transactions which are posted to the mainchain (via the Bridge), and
  later participants may claim `FUEL` from a specified Merkle root by minting
  `FUEL` tokens.

The key point is this -- the current mechanism implements this transfer via
*minting* and *burning* of some distinguished token.
We call such a token a *wrapped token*.

In general, the mechanism for which coins are transferred between sidechains
and back at a fixed (or otherwise deterministic) exchange rate is a called a
*two-way peg*[^refEnablingBlockchainInnovations].

We call the currently implemented mechanism which implements the two-way peg
via minting and burning of a wrapped token a *wrapped token two-way peg*.

This SIP proposes an alternative method (which may be used in place of the
*wrapped token*) to implement the two-way peg.
Instead of minting and burning a wrapped token, we propose to take any
arbitrary token on the mainchain, say `MCToken`, and implement the two-way peg
as follows.

- Transfers of `MCToken` from *mainchain* to *sidechain* amounts to
  participants sending their `MCToken` to a distinguished address that contains
  the sidechain recipient (as datum), call this address a *lock box address*,
  which *locks* the participant's `MCToken`.
  So for sidechain recipient to receive  their corresponding sidechain tokens
  in the sidechain, the Bridge must observe transactions which lock `MCToken`s
  at a lock box address and mint the corresponding amount to the sidechain
  recipient.

- Transfers of `MCToken` from *sidechain* to *mainchain* amounts to (again)
  creating a Merkle root of such transactions which are posted to the mainchain
  (note this reuses the existing mechanism),
  and later participants may claim their `MCToken` by using some specified
  Merkle root to spend UTxOs at a lock box address for which `MCToken` is
  locked at and finally pay the locked `MCToken`s to themselves.
  In essence, Merkle roots give participants permission to *unlock* previously
  locked `MCToken` to claim it themselves.

We call such a two-way peg implemented this way a *lock/unlock two-way peg*, and
the implementation of a lock/unlock two-way peg is the primary focus of this
proposal.
We will also discuss generalizations.

## Design Overview
We will present two designs to implement a lock/unlock two-way peg.
Both designs will follow the same steps when implementing it.

1. Defining a validator `LockBoxValidator` which as datum has at least (it
   indeed may contain more data) the sidechain recipient, which will be used
   as the lock box address i.e., a transfer to the mainchain to the sidechain
   amounts to a user paying tokens to the `LockBoxValidator` address.

2. Defining a validator `LockBoxValidator` which as datum has at least (it

## Plutus Specification Design: Proxied Unlocking
TODO: mention that it needs to be checked that `LockBoxValidator` is
"relatively" small.

This design will have a `LockBoxValidator` validator which will be the lock box
address for `MCToken`s.
Unlike the original specification where participants claimed `FUEL` tokens from
Merkle roots, participants will instead claim `LockProxyPolicy` tokens for
which burning `LockProxyPolicy` allows unlocking `MCToken`s residing at
`LockBoxValidator` addresses.
Moreover, to ensure that mainchain recipients may only claim at most the number
of `MCToken` specified from the sidechain, we will also require
a validator, `LockConfigValidator`, which holds as datum information regarding
`LockProxyPolicy` and `LockBoxValidator` (to avoid circular dependencies); and
we will also need an NFT `LockConfigOraclePolicy` which will uniquely identify
such a `LockConfigValidator`.

In summary, the design will require the following Plutus scripts.
- `LockBoxValidator`: the validator address which will be the lock box address.
- `LockProxyPolicy`: a minting policy which is minted from Merkle roots and
  whose burning unlocks `LockBoxValidator`.
- `LockConfigValidator`: a validator which always returns false which as
  datum contains necessary information to handle the circular dependencies
  between `LockBoxValidator` and `LockProxyPolicy`.
- `LockConfigOraclePolicy`: an NFT to uniquely identify `LockConfigValidator`.

We define these scripts more precisely.

`LockConfigOraclePolicy` is an NFT (and hence must be parameterized by a UTxO)
that must be paid to a `LockConfigValidator` (which it uniquely identifies).
`LockConfigValidator` never succeeds and has as datum
```haskell
data LockConfigDatum = LockConfigDatum
    { lockBoxValidatorAddress :: Address
    , lockProxyPolicyCurrencySymbol :: CurrencySymbol
    , lockedMcToken :: CurrencySymbol
    }
```
which contains the address of `LockBoxValidator`, currency symbol of
`LockProxyPolicy`, and the currency symbol of the `MCToken`.

In a sense, `LockConfigValidator` and `LockConfigOraclePolicy` provide a
static read-only configuration of this system that *must* be run first to
initialize the system.

`LockConfigValidator` will be parameterized by `LockConfigOraclePolicy` and
validates only if all of the following are satisfied:
-  there exists a reference input which holds `LockConfigOraclePolicy` that has
   as datum `LockConfigDatum`; and
-  `lockProxyPolicyCurrencySymbol` burns at least one token (i.e., mints
   strictly less than 0 tokens).
Note that `LockConfigValidator` simply forwards all of its verifications to
`LockProxyPolicy` which does most of the heavy lifting.

Recall that `LockProxyPolicy` will replace `FUELMintingPolicy`.
Moreover, recall that `FUELMintingPolicy` mints `k` tokens with token name
`FUEL` only if `k` tokens were transferred over to the mainchain recipient from
the sidechain, and `FUELMintingPolicy` may be burned arbitrarily.
`LockProxyPolicy` will be essentially identical to `FUELMintingPolicy` except
that minting will be generalized to allow for different token names, and
`LockProxyPolicy` will burn `k` tokens with some token name `tn` only if `k`
`MCToken`s with token name `tn` are claimed from `LockBoxValidator`.

More precisely, `LockProxyPolicy` will be parameterized by

- `SidechainParams`;

- the currency symbol of
  [`MerkleRootTokenMintingPolicy`](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#3-transfer-fuel-tokens-from-sidechain-to-mainchain)
  to ensure that sidechain transactions have been previously signed by the
  committee;

- the currency symbol of [`DsKeyPolicy` for keys in the distributed set](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/DistributedSet.md).

- the currency symbol of `LockConfigOraclePolicy` for identifying
  `LockConfigValidator` which contains as datum `LockConfigDatum`.

Note that `LockProxyPolicy` is parameterized by the same things
`FUELMintingPolicy` except for the extra last currency symbol.

As redeemer, `LockProxyPolicy` will take the following data type.
```haskell
data LockProxyRedeemer
  = LockProxyMint
    { lockProxyMintMerkleTreeEntry :: MerkleTreeEntry
    , lockProxyMintMerkleProof :: MerkleProof
    }
  | LockProxyBurn
```
Again, note the similarities to `FUELRedeemer` from the main specification --
`LockProxyMint` is identical to `SideToMain`, but `LockProxyBurn` is different
as burning `LockProxyPolicy` does *not* correspond to transferring tokens from
mainchain to sidechain.

Now, we discuss the conditions for which `LockProxyPolicy` mints.
We first propose a minor `MerkleTreeEntry`s (i.e., transactions sent over from
the sidechain).
Since `MCToken` may have an arbitrary token name and the current
`MerkleTreeEntry` assumes the token name is `FUEL`, we propose to modify
`MerkleTreeEntry` from

```haskell
data MerkleTreeEntry = MerkleTreeEntry
  { index :: Integer -- 32 bit unsigned integer, used to provide uniqueness among transactions within the tree
  , amount :: Integer -- 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
  , recipient :: ByteString -- arbitrary length bytestring that represents decoded bech32 cardano address
  , previousMerkleRoot :: Maybe ByteString -- previousMerkleRoot is added to make sure that the hashed entry is unique
  }
```
to
```haskell
data MerkleTreeEntry = MerkleTreeEntry
  { -- | unsigned integer, used to provide uniqueness among transactions within the tree
    index :: Integer
  , -- | 256 bit unsigned integer that represents amount of tokens being sent out of the bridge
    amount :: Integer
  , -- | Token name of the asset in the mainchain
    tokenName :: TokenName
  , -- | arbitrary length bytestring that represents decoded bech32 cardano address
    recipient :: Address
  , -- | ensures that the hashed entry is unique (between Merkle trees)
    previousMerkleRoot :: Maybe ByteString
  }
```
Note the following.
- There's an extra `tokenName` field which is the token name of the `MCToken`
  that will be claimed on the mainchain.
- [optional change] we changed the type of `recipient` to `Address` since the
  current onchain code parses a limited subset of bech32 Cardano addresses and
  it would (most likely) be a bit more efficient if we removed this parsing
  problem altogether and provided data in the right format in the offchain
  code.
  This change potentially increases efficiency, and also allows someone else to
  claim your tokens for you.
  [See #280 for details.](https://github.com/mlabs-haskell/trustless-sidechain/issues/290)
- [optional change] We fixed the documentation so it matches Haskell's standard
  Haddock comments

Then, `LockProxyPolicy` mints (strictly more than 0 tokens) only if all of the
following are satisfied.

- The redeemer is `LockProxyMint`.
- A `MerkleRootToken` with token name, say `merkleRoot`, is given as a
  reference input at a `MerkleRootTokenValidator` script address.
- `blake2b(cbor(lockProxyMintMerkleTreeEntry))` is in the Merkle root
  `merkleRoot` using witness the Merkle proof`lockProxyMintMerkleProof`.
- `blake2b(cbor(lockProxyMintMerkleTreeEntry))` is NOT included in the
  [distributed set](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/DistributedSet.md),
  and is inserted in the distributed set in this transaction (to ensure that tokens can be claimed at most once).
- The transaction corresponds to the `lockProxyMintMerkleTreeEntry` in the
  sense that: `LockProxyMint` mints exactly `amount` tokens with unique token
  name `tokenName`; and there exists a transaction output at `recipient` with
  at least `amount` `LockProxyMint` tokens with token name `tokenName`
  (see [#280 for the security proof](https://github.com/mlabs-haskell/trustless-sidechain/issues/290)).

Again, note that this is essentially identical to the conditions given in
[`FUELMintingPolicy` for individual
claiming](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md#32-individual-claiming)
except for the token name verifications and the `recipient` adjustments.

Finally, `LockProxyPolicy` with token name `tokenName` burns (mints strictly
less than 0 tokens) `k > 0` tokens only if all of the following are satisfied.
- There exists a reference input with `LockConfigOraclePolicy` which has as
  datum `LockConfigDatum`.
- Let `ki` denote the sum of transaction *inputs* with currency symbol
  `lockedMcToken` and token name `tokenName` at address
  `lockBoxValidatorAddress`.
  Let `ko` denote the sum of all transaction *outputs* with currency symbol
  `lockedMcToken` and token name `tokenName` at address
  `lockBoxValidatorAddress`.
  Then, we verify that `ko = ki - k` i.e., exactly `k` `MCToken`s with token
  name `tokenName` were unlocked from the provided `LockBoxValidator`s, and the
  rest are left to be claimed by someone else.

Obviously, this satisfies the requirements of a two-way peg.

TODO: restate the *entire* workflow for completeness.

### Proxied Unlocking Design Discussion
The good
- it will always work

The bad
- potentially bad concurrency
- need to implement essentialyl coin balancing / wallet balancing algorithms
  for `MCToken`s at `LockBoxValidator`.

Further work
- Multiple assets (shouldn't be too tricky I hope? Need to give it a bit more
  thought)
- general data bridge (this "just" haha decays down configuring which token is
  minted when claiming)

## Design Motivation
TODO: oh gosh this is probably important to justify things

This section will discuss some design considerations inherent to the Cardano
blockchain when designing the lock/unlock two-way peg.

Consider the following scenario.

1. Alice transfers 1 `MCToken` from the mainchain to the sidechain 100 times
   with her sidechain public key as the recipient i.e., there are 100 UTxOs on
   the mainchain with a single `MCToken` at a lock box address.

2. Correspondingly, the sidechain mints 100 tokens, say `SCToken`, (one at a
   time) which Alice has possession of in the sidechain.

3. In the sidechain, Bob purchases all 100 of Alice's `SCToken`s.

4. Bob transfers 100 of his `SCToken`s in the sidechain to mainchain.

Note that when Bob attempts to claim his newly bought `SCToken`s from Alice in
the mainchain that Bob has just transferred over from the sidechain, Bob
*cannot* claim all such tokens in a single transaction due to transaction size
limitations in Cardano.
In this case, a transaction with 100 UTxOs as transaction inputs to claim all
100 locked `MCToken`s is infeasible.

## Design: Direct Unlocking
TODO: motivate this, discuss why it may optimize, discuss the UTxO merging
problem.

## Generalizations

## Related Work
TODO: this is all unreadable garbage  not too sure where to put his exactly.
also don't forget to mention the original white paper

Unsurprisingly, lock/unlock two-way pegs for sidechains have been implemented
before.

We do a quick overview of some implementations (changing notation to be
consistent with this document) for how some lock/unlock two-way pegs were
implemented, and discuss tradeoffs in the context of our work here.
We will note that all mechanisms implement the transfer from the mainchain to
the sidechain via paying tokens to a lock box address, so the only difficulty
is implementing the sidechain to mainchain transfer.

- Drivechain[^refDrivechain] for BitCoin sets the lock box address to be an
  "anyone can spend" address, so via a soft fork, it miners are trusted to
  ensure that the locked `MCToken`s (bitcoins) are only unlocked in

  Via a soft fork, when sufficient sidechain to mainchain transfers appear,
  miners will be trusted to bundle locked `MCToken`s in a lock box address into
  a *single large UTxO*, then pay this single large UTxO to claim addresses.

  This is essentially the [Directly Unlocking
  design](#design:directly-unlocking) where note that the role of the miners is
  replaced by Plutus smart contracts.

- XClaim[^refXClaim] sets the lock box address to a backing intermediary's
  address who is incentivized to act honestly and allow participants to claim
  their `MCToken` (transferred from sidechain to mainchain) by forcing the
  backing intermediary to deposit collateral which is slashed and reimbursed to
  wrong actors if the backing intermediary behaves incorrectly.

  Indeed, it is possible to replace backing intermediary with smart contracts
  as was taken in these designs.

- Polkadot[^refPolkadot] TODO: fill me in later.

[^refEnablingBlockchainInnovations]: Back, A., Corallo, M., Dashjr, L.,
Friedenbach, M., Maxwell, G., Miller, A.K., Poelstra, A., Timón, J., & Wuille,
P. (2014). Enabling Blockchain Innovations with Pegged Sidechains.

[^refDrivechain] P. Sztorc. Drivechain - The Simple Two Way Peg, November 2015.
<http://www.truthcoin.info/blog/drivechain/>

[^refXClaim]: Zamyatin, Alexei, et al. "XCLAIM: Trustless, Interoperable,
  Cryptocurrency-Backed Assets." 2019 IEEE Symposium on Security and Privacy
  (SP), IEEE, 2019, pp. 193–210, <https://doi.org/10.1109/SP.2019.00085>.

[^refPolkadot] <https://assets.polkadot.network/Polkadot-whitepaper.pdf>
