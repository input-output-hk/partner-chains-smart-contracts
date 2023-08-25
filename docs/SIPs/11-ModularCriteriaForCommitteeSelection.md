# Modular criteria for committee selection

## Goals
1. Two Sidechains can be created and run on a target Cardano network, one using
Ada stake distribution, the other using ownership of a native token
2. Rotation works as expected - moving between permissioned candidates and
candidates based on and stake distribution
3. Changes in ownership are observable on the sidechain
4. Registration is adjusted to support both flows

## Background

The current implementation of the Sidechain protocol is based on the following
workflow:
1. A mainchain SPO registers to be Sidechain Committee Candidate
2. A registration observed and verified by the Bridge
3. When enough valid registrations are accumulated, the Sidechain is activated

A candidate is only accepted if it is a valid SPOs. However, due to limitations
of the Plutus language and the script context we can access in on-chain validators,
we cannot verify the staked amount onchain. These verifications will thus all
happen on the Bridge, while mainchain is only responsible for accepting and
storing registrations.

After sufficient registrations have occurred, there is a set of Sidechain
Committee Candidates who may be elected to create blocks in the sidechain.
Indeed, by design of proof-of-stake blockchains, the election of a particular
Sidechain Committee Candidate to create a block for a given epoch and slot
(call this process a *leader election*) is proportional to the amount of stake
they own. There is a design space to consider the stake owned on the mainchain
and the sidechain, but in this SIP we will only discuss stake owned on the
mainchain.

The stake currently used for the sidechain leader election is based on Ada
ownership on the mainchain. Moreover, this leader election process is exclusively
an offchain process i.e., the onchain code is completely unaware of the Sidechain
Committee Candidates' stake (Ada ownership).

This proposal discusses how one may generalize which tokens are used for the
sidechain leader election on the mainchain.

Clearly, we would like to keep the existing infrastructure where the leader
election process happens offchain -- doing this onchain would require significant
changes to existing code and is a nontrivial process. Hence, it's clear that
generalizing the tokens used for the sidechain leader election will require
similar offchain observation to track ownership of tokens.

In case of a native token based committee selection, we will use an arbitrary
predefined token as the basis for committee selection. For further reference
in this document we well call this token a `StakeToken`, however this can be any
Cardano native token. In fact, this can even be the same as the `CandidatePermissionToken`,
or could be any other token that was not minted by the sidechain contracts.

There are two possible approaches to stake ownership verification:

#### 1. Observing CommitteeCandidateValidator

To make observation of stake ownership easy, these native tokens must be locked
at registration to the CommitteeCandidateValidator address, and the offchain
code must observe the locked amount on this address. Observing this address
is already implemented by the Sidechain Bridge in order to register candidates,
so this is a relatively simple addition.

The disadvantage of this approach is that the committee member cannot easily
trade its staked tokens: changes of stake ownership amounts to deregistering,
trading the tokens, then registering again.

#### 2. Observing committee member address

The Bridge implements direct observation of addresses of all committee members
to verify stake ownership where committee candidate registration is used to
uniquely associate a mainchain public key's stake ownership to a sidechain
public key.

This a superior approach in terms of user experience, however implementation
costs on the Sidechain Bridge might make this less ideal.

## Implementation

Block producer registration will require different data depending on the
staking method. To represent this in the `BlockProducerRegistration` datum,
we will introduce a new data type:

```haskell
data StakeOwnership
  = -- | Ada stake based configuration comprises the SPO public key and signature
    AdaBasedStaking PubKey Signature
  | -- | Token based staking configuration
    TokenBasedStaking
```

And replacing direct references to SPO public key and signature with
`StakeOwnership`.

```diff
data BlockProducerRegistration = BlockProducerRegistration
- { -- | SPO cold verification key hash
-   -- | @since Unreleased
-   spoPubKey :: PubKey -- own cold verification key hash
+ { stakeOwnership :: StakeOwnership
  , -- | public key in the sidechain's desired format
    -- | @since Unreleased
    sidechainPubKey :: LedgerBytes
- , -- | Signature of the SPO
-   -- | @since Unreleased
-   spoSignature :: Signature
  , -- | Signature of the sidechain
    -- | @since Unreleased
    sidechainSignature :: Signature
  , -- | A UTxO that must be spent by the transaction
    -- | @since Unreleased
    inputUtxo :: TxOutRef
  , -- | Owner public key hash
    -- | @since Unreleased
    ownPkh :: PubKeyHash
  }
```

In case of the `CommitteeCandidateValidator` observation approach, the Sidechain
Bridge must be aware of the currency symbol and token name of
the `StakeToken` and verify the locked amount in the registration UTxO.

Furthermore, off-chain infrastructure and the sidechain CLI endpoint for
registration endpoint will enable us to choose the committee selection module.
Additional arguments:
`committee-selection`, `stake-token-currency`, `stake-token-name`,
`stake-token-amount`

*Ada stake module:*

```
sidechain-main-cli -- register \
  --payment-signing-key-file SIGNING_KEY_FILE \
  --genesis-committee-hash-utxo TX_ID#TX_IDX \
  --sidechain-id 1 \
  --sidechain-genesis-hash GENESIS_HASH \
  --threshold-numerator 2 \
  --threshold-denominator 3 \
  --committee-selection ada-stake \
  --atms-kind plain-ecdsa-secp256k1 \
  --spo-public-key PUBLIC_KEY \
  --sidechain-public-key PUBLIC_KEY \
  --spo-signature SIGNATURE \
  --sidechain-signature SIGNATURE \
  --registration-utxo TX_ID#TX_IDX
```

*Native token module:*

```
sidechain-main-cli -- register \
  --payment-signing-key-file SIGNING_KEY_FILE \
  --genesis-committee-hash-utxo TX_ID#TX_IDX \
  --sidechain-id 1 \
  --sidechain-genesis-hash GENESIS_HASH \
  --threshold-numerator INT \
  --threshold-denominator INT \
  --committee-selection native-token-stake \
  --stake-token-currency symbol CUR_SYM \
  --stake-token-name TOKEN_NAME \
  --stake-token-amount AMOUNT \
  --atms-kind plain-ecdsa-secp256k1 \
  --sidechain-public-key PUBLIC_KEY \
  --sidechain-signature SIGNATURE \
  --registration-utxo TX_ID#TX_IDX
```

For the committee member observation approach, no further action is required,
the Sidechain Bridge can use the `ownPkh` to verify stake ownership.
