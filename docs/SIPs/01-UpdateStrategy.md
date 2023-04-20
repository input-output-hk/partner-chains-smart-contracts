# Update strategy

In this specification we describe the mechanisms of the Cardano mainchain part of the trustless
sidechain protocol update. To allow the protocol to evolve with time, we need to migrate from old
validators to new ones, deprecate old tokens to new ones.

We achieve the update by using an oracle validator. This validator address will be the single
source of truth, to find the validators and minting policies for the protocol.

## 1. Assumptions

- The governance strategy required to approve the protocol update is out of scope of this specification

## 2. Strategies

We considered the following strategies, but these can also be used in combination:

- _migration strategy_: at any given point in time, there's only one valid version of a
  validator or minting policy. On an update event, all assets have to be migrated to the new
  protocol. The benefit of this approach is the simplicity, however the migration cost of a
  long-living chain can be tremendously costly.
- _versioned update_: in this strategy, we would maintain references to old validators and minting
  policies, giving time for the protocol users to migrate their assets as they like. With this
  strategy it is important to allow full migration, in case a version has to be abandoned due to
  a security issue. The update method must be flexible enough to allow addition and removal of
  certain validators/minting policies.
  For versioned validators, the version used must be specified by the datum or redeemer of the
  transaction.
- _partial versioned update_: this is in-between migration and the versioned solution, where the
  UTxOs in the old validator are considered valid after the update, but new insertions are not
  allowed. For partially verioned validators, always the latest version is used by default.
  In practice these validators are set up the same way as the ones with migration strategy, but
  without having the old tokens and datums migrated to the new version of the validator.
- _transaction token pattern_: we could introduce a light-weight validator/minting policy in place
  of our current minting policies and validators, and move all actual logic to
  [Transaction Token Minting Policies (TxTMP)](https://plutonomicon.github.io/plutonomicon/transaction-token-pattern).
  The validators and minting policies could then use an oracle to reference the current version(s)
  of a TxTMP that is accepted, and would only verify that the referenced token is minted.
  The benefits of this approach are:

  - our validator addresses and currency policies would be constant over versions
  - decoupled token minting and burning logic (a token minted with V1 logic can be burnt with V2, etc.)

  A drawback is that this would slightly raise the fees due to the cost of extra tokens minted for each FUEL minting transaction.

These strategies can be used in combination for optimal migration cost/complexity.

In case of our sidechain protocol, I propose the following strategies for our validators and minting policies:

- `FUELMintingPolicy`: Transaction Token Pattern
- `CommitteeCandidateValidator`: migration
- `MPTRootTokenMintingPolicy`: partial versioned update
- `MPTRootTokenValidator`: partial versioned update
- `CommitteeHashPolicy`: partial versioned update
- `CommitteeHashValidator`: partial versioned update
- `DsConfValidator`: it's role is taken over by `VersionOracleValidator`
- `DsConfPolicy`: it's role is taken over by `VersionOraclePolicy`
- `DsInsertValidator`: versioned update
- `DsKeyPolicy`: versioned update

## 3. FUELMintingPolicy Transaction Token Pattern Implementation:
This section discusses in more detail how to apply the transaction token
pattern to the FUELMintingPolicy.
In the development of these ideas, we will later introduce a new minting
policy, `FUELProxyPolicy`, whose tokens will be regarded as the FUEL
tokens.

As a high level idea, the Transaction Token Pattern will change the
FUELMintingPolicy to forward all of its validations to some
*collection of minting policies* stored in the datum at a distinguished UTxO, and
the FUELMintingPolicy will mint only if the aforementioned collection of
minting policies *all* mint in the same transaction. The extra tokens minted
from the aforementioned minting policies may be paid to any address and does
not effect the functionality of the system[^extraTokensNote].

[^extraTokensNote]:
    The extra tokens could be sent directly back to the user's wallet, or paid
    to some arbitrary validator script. The former option makes this confusing
    for users, and the latter scenario will slightly increase the cost of the
    transaction (min UTxO fee).

Provided we have sufficient governance mechanisms, one may arbitrarily change
the distinguished UTxO which holds a collection of minting policies to modify
the sufficient conditions for the FUELMintingPolicy to mint. In essence, this
allows one to upgrade the FUELMintingPolicy without requiring changes to
existing FUEL already stored onchain i.e., there is no need to migrate existing
tokens over to a new upgraded FUELMintingPolicy which can be costly (in terms
of ada) if there are already many FUEL tokens existing onchain.

Moreover, since the FUELMintingPolicy will mint only if some collection of
minting policies *all* mint, this provides some modularity onchain with regards
to the sufficient conditions for the FUELMintingPolicy in the following sense:

- If an upgrade would like to modify an existing minting policy in the
  collection of minting policies, this amounts to replacing that minting policy
  with a new one.

- If an upgrade would like to add a minting policy to a collection of minting
  policies (in effect, this adds more required conditions to mint a FUEL
  token), then this amounts to adding a minting policy to some collection of
  minting policies in the UTxO.

### 3.3 Alternative designs
We discuss alternative designs.

We classify the designs in two categories: explicit token migration, and
implicit token migration.

- _Explicit token migration._ Suppose that one would like to modify the system
  for an upgrade.
  Indeed, this forces a change in the current system's validators and minting
  policies; so participants must agree what the new `FUEL` tokens are, and
  explicitly burn their current `FUEL` tokens in exchange for the new `FUEL`
  tokens. We call this process of participants explicitly burning their assets
  in exchange for a new version of their assets an *explicit token migration*.

  We discuss various ways of implementing the explicit token migration.

    - _Allowing individuals to migrate tokens themselves._ This could be
      achieved by implementing the upgraded FUEL minting policy to
      alternatively mint only if the old FUEL is burnt. Then, users could
      individually claim their new FUEL via burning their own old FUEL.

      Note that the new `FUEL` minting policy needs to support the migration of
      assets, and would hence be a larger than necessary as it must support
      migration of the old `FUEL`. As more updates happen, this could make
      supporting many old versions impossible due to transaction size limits.

    - _Using the Bridge to migrate tokens._ Recalling that the Bridge already
      observes changes in the main chain, we may modify the Bridge / `FUEL`
      claiming mechanism to allow new `FUEL` to be claimed in the case that old
      `FUEL` is burnt.

_Problems with explicit token migration._ If there's a critical security issue
with the old FUEL (e.g. someone can mint themselves unlimited FUEL), then this
critical security issue also appears in the new `FUEL`. Moreover, if this
system runs for a long time, it would be costly (in terms of ada) for every old
`FUEL` to be burnt and minted to new `FUEL`.

- _Implicit token migration._ In
  [3.](#3-fuelmintingpolicy-transaction-token-pattern-implementation) we
  described a method to upgrade the system without requiring participants to
  migrate their assets via burning / minting to the new system.
  Indeed, an upgrade with such a system requires no actions from participants,
  and we call this an *implicit token migration*.

_Problems with implicit token migration._ Since the method of implicit token
migration introduces additional minting policies and validators inside of the
transaction, this certainly will make things cost more (in terms of ada). So,
this has the potential issue of running into transaction size limits, but
reference scripts ([CIP33](https://cips.cardano.org/cips/cip33/)) can help
mitigate this.

_Discussion._ Clearly, both of the presented methods have their shortcomings.
In our opinion, we believe that the implicit token migration using the
    transaction token minting policy pattern is the superior choice.
It provides us with the ability to migrate tokens for free, and provides us with the flexibility
    to modify the system in production should there need changes (upgrades, security patches, etc.).
Also, it provides a clean break of abstraction to decouple behavior.

### 3.4 FUELMintingPolicy Validators / Minting Policies
Let `FUELMintingPolicy` denote the existing FUELMintingPolicy in the system.
See the original Plutus contract specification for details
[here](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md).

We will modify the `FUELMintingPolicy` to ensure that a given *claim
certificate* is only valid for one version of the policy. In particular, we
parameterise the `FUELMintingPolicy` with a `version :: Int`, and introduce a
new `version` field in the `MerkleTreeEntry`. When verifying the
`SignedMerkleProof` (which contains the hash of the `MerkleTreeEntry`), the
policy has to check that the version of the script matches the version of the
certificate. See [4.](#4-versioning-implementation).

We will introduce a new `FUELProxyPolicy` which will be regarded as the `FUEL`
tokens. `FUELProxyPolicy` will be parameterized two currency symbols,
`FUELOracleMintPolicy` and `FUELOracleBurnPolicy`, which are NFTs (and hence
must be parameterized by a UTxO) that will uniquely identify distinct UTxOs at
a script validator address `FUELOracleValidator` which holds as datum some
collection of minting policies that completely determines that conditions when
`FUELProxyPolicy` mints / burns.

An upgrade to the system amounts to spending the `FUELOracleValidator` that the
NFT `FUELOracleMintPolicy` (or `FUELOracleBurnPolicy`) sits at, and paying the
NFT `FUELOracleMintPolicy` (or `FUELOracleBurnPolicy`) to a new
`FUELOracleValidator` with an altered collection of minting policies as datum.
Then, the conditions for when `FUELProxyPolicy` mints is now determined by the
new altered collection of minting policies. It is outside the scope of this
document to discuss the predicates for when the validator `FUELOracleValidator`
will succeed, as there would need to be some sort of governance mechanism which
decides when we may upgrade `FUELProxyPolicy`. See
[3.5](#35-governance-of-updates) for details.

The rest this section discusses the `FUELProxyPolicy` design for how it
determines which collection of minting policies must mint in order for
`FUELProxyPolicy` to itself mint.

We first need a data type, `ProxiedTokens`, which stores a collection of
minting policies as currency symbols.
```
data ProxiedTokens =
    { amountToken :: CurrencySymbol
        -- ^ 'amountToken' is the distinguished currency symbol which
        -- determines how many 'FUELMintingPolicy' is minted.
    , extraTokens :: [CurrencySymbol]
        -- ^ 'extraTokens' are the extra tokens that must mint in the
        -- same transaction in for 'FUELMintingPolicy' to be minted.
        -- We require that 'extraTokens' is sorted lexicographically.
    }
```
We say that a given *`ProxiedTokens` mints `k` tokens* (or just
*`ProxiedTokens` mints* if `k` is irrelevant) if in the same transaction:

- `amountToken`  mints `k` tokens (of token name `""`); and

- every token in `extraTokens` also mints.

Note that we require that `extraTokens` is sorted lexicographically to
make onchain verifications easier[^extraTokensOnchainVerifications].

[^extraTokensOnchainVerifications]:
    Recall:
        1. [Onchain maps are sorted lexicographically by their
           keys](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/test-suite/cddl-files/alonzo.cddl#L110-L113).
        2. [Values minted in a transaction, call this `mint`, are represented
           with a
           map](https://github.com/input-output-hk/cardano-ledger/blob/master/eras/alonzo/test-suite/cddl-files/alonzo.cddl#L379-L384)
           and hence are sorted lexicographically by their keys.
        3. [Maps in Plutus are represented by lists of tuples (key value pairs) where the lookup operation has linear complexity](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/AssocMap.hs#L131-L139).
    So, if the `extraTokens` are sorted, one can do an efficient linear scan
    which simultaneously goes through both the `extraTokens` and the `mint` map
    to ensure that the corresponding values are actually minted in the
    transaction.

Thus, `FUELProxyPolicy` will mint `k > 0` tokens named `FUEL`, only if it is
the case that a given `ProxiedTokens` (in a datum of a UTxO identified by the
NFT `FUELOracleMintPolicy` given as a reference input) mints `k` tokens. Note
that we require that `k > 0` for now, and we will return to the case when `k <
0` when burning `FUELProxyPolicy` shortly.

Indeed, if we intended every upgrade to invalidate all old versions, or only
wanted one version active at any point in time, then it would be sufficient to
just store a single `ProxiedTokens` as datum of a validator identified by the
NFT `FUELOracleMintPolicy` to determine the conditions when `FUELProxyPolicy`
mints.
Unfortunately, we would like to support multiple versions to be active at the
same time, so we propose to store a collection of `ProxiedTokens` in the UTxO
uniquely identified by the NFT `FUELOracleMintPolicy`, and if *any* of the
`ProxiedTokens` mints, then `FUELProxyPolicy` also mints.

Since doing a linear search of testing which `ProxiedTokens` mints in the
collection is inefficient, we will index each of the `ProxiedTokens` with a
`Version` to uniquely identify it and participants must provide a `Version` (as
redeemer) to determine which `ProxiedTokens` they wish to use to claim their
`FUELProxyPolicy`.

So, the collection of `ProxiedTokens` identified by the NFT
`FUELOracleMintPolicy` will reside as datum at a script validator address,
`FUELOracleValidator`, with datum `FUELOracleDatum`, defined as follows.
```
type FUELOracleDatum = VersionMap

newtype Version = Version Integer

newtype VersionMap = VersionMap
    { unFUELOracleDatum :: Map Version ProxiedTokens
        -- ^ this maps 'Version's to 'ProxiedTokens' where participants may
        -- choose a 'Version' with its corresponding 'ProxiedTokens' that they
        -- would like to use to claim their 'FUELProxyPolicy' tokens.
    }
```
That is, `FUELOracleDatum`, is a partial map from `Version`s to `ProxiedTokens`.
See [here](https://github.com/input-output-hk/plutus/blob/master/plutus-tx/src/PlutusTx/AssocMap.hs#L58-L60)
for the `Map` data type.

When a participant wishes to claim `FUELProxyPolicy`, they need to provide as
redeemer the `Version` which corresponds to the `ProxiedTokens` they wish to
use to claim their tokens.
So, `FUELMintingPolicy` must have as redeemer
```
data FUELProxyPolicyRedeemer = FUELProxyPolicyRedeemer
    { version :: Version
        -- ^ 'version' determines which 'ProxiedTokens' the participant is
        -- interested in the 'FUELOracleDatum'.
    , mode :: Mode
        -- ^ 'mode' determines whether the participant would like to mint or
        -- burn tokens.
    }

data Mode
    = Mint
    | Burn
```
where the `version` is used in the transaction to identify the `ProxiedTokens`
in the `FUELOracleDatum` that `FUELProxyPolicy` will verify has minted, and the
`mode` is used to determine whether the participant is interested in minting or
burning `FUELProxyPolicy`.

If the `mode` is `Mint`, then `FUELProxyPolicy` will look in its reference
inputs for a UTxO which is uniquely identified by `FUELOracleMintPolicy` with
datum as `FUELOracleDatum` and verify that the `ProxiedTokens` corresponding to
the given `version` in the `FUELOracleDatum` mints.

Otherwise, if the `mode` is `Burn`, similar to the `Mint` case,
`FUELProxyPolicy` will look in its reference inputs for a UTxO which is
uniquely identified by `FUELOracleBurnPolicy` with datum `FUELOracleDatum`.
But, the mechanism to determine how many tokens to burn differs.

Unlike minting, where one may arbitrarily mint tokens iff the minting policy
succeeds, burning tokens requires the participant to additionally spend such
tokens from an address. This is inconvenient since the `amountToken` in any
given `ProxiedTokens` must be present in some address when attempting to burn,
but as time goes on, updates may invalidate such tokens.

So, recall that we parameterized `FUELProxyPolicy` with the additional
currency symbol `FUELOracleBurnPolicy` that serves the same purpose as
`FUELOracleMintPolicy` (i.e., `FUELOracleBurnPolicy` is an NFT which uniquely
identifies a UTxO with a `FUELOracleDatum`), except that `FUELProxyPolicy` will
*burn* `k > 0` (i.e., mint `-k` tokens) only if a `ProxiedTokens` in the
`FUELOracleDatum` mints `k` tokens in the same transaction.

Clearly, this removes the need to spend particular tokens when burning
`FUELProxyPolicy`.

_Summary_

To summarize, we write the details of `FUELProxyPolicy` in full.
`FUELProxyPolicy` will be parameterized by the currency symbols of the NFTs
`FUELOracleMintPolicy` and `FUELOracleBurnPolicy`, and takes as redeemer a
`FUELProxyPolicyRedeemer`.

If the the `mode` of the redeemer is `Mint`, `FUELProxyPolicy` verifies all of the following:

1. there is a reference input in the current transaction which contains the
  `FUELOracleMintPolicy` token with `FUELOracleDatum` as datum;

2. the minted `FUELProxyPolicy` only has token name `FUEL`;

3. there is a value, `proxiedTokens`, in `FUELOracleDatum` which corresponds to
  the `version` of the redeemer ;

4. the `amountToken` in `proxiedTokens` mints `k > 0` tokens with token name `""`;

5. `FUELProxyPolicy` also mints `k` tokens; and

6. every currency symbol `c` in the `extraTokens` in `proxiedTokens`, mints at
  least one token.

If the the `mode` of the redeemer is `Burn`, `FUELProxyPolicy` verifies all of the
following (note conditions 2., 3., 6. are the same as before):

1. there is a reference input in the current transaction which contains the
  `FUELOracleBurnPolicy` token with `FUELOracleDatum` as datum;

2. the minted `FUELProxyPolicy` only has token name `FUEL`;

3. there is a value, `proxiedTokens`, in `FUELOracleDatum` which corresponds to
  the `version` of the redeemer;

4. the `amountToken` in `proxiedTokens` mints `k > 0` tokens with token name `""`;

5. `FUELProxyPolicy` also mints `-k` tokens (i.e., burns `k` tokens); and

6. every currency symbol `c` in the `extraTokens` in `proxiedTokens`, mints at
  least one token.

Otherwise, `FUELProxyPolicy` fails.

The entire workflow is summarized as follows.

**Workflow**
1. NFTs `FUELOracleMintPolicy` and `FUELOracleBurnPolicy` are minted, and paid
   to distinct `FUELOracleValidator` validator scripts with datums
```
    0 -> FUELOracleDatum
        { amountToken = Currency symbol of FUELMintingPolicy
        , extraTokens = []
        }
```
and
```
    0 -> FUELOracleDatum
        { amountToken = Currency symbol of a tautology policy that always returns true
        , extraTokens = []
        }
```
respectively where we use the notation `a -> b` to denote "`a` maps to `b`".
Note the choice `0` as the version number was arbitrary.

2. Users may mint `FUELProxyPolicy` for `FUEL` where we note that we have
   `FUELProxyPolicy` minting only if `FUELMintingPolicy` mints; and similarly, users may
   burn `FUELProxy` only if the tautology policy returns true (i.e., always).

3. A governance mechanism chooses to upgrade the system by spending the UTxO
   that holds the `FUELOracleMintPolicy` NFT at validator
   `FUELOracleValidator`, paying it to another `FUELOracleValidator` validator
   address with a new `FUELOracleDatum`.
   Indeed, the same upgrade mechanism applies for upgrading the burning
   mechanism as well.

4. Note that any new `FUEL` tokens must now validate with the new collection of
   minting policies provided.

5. Steps 2., 3., 4. may be repeated indefinitely for users and governance mechanisms.

![FUELProxy mint flow](01-UpdateStrategy/FUELProxyMint.svg)

<figcaption align = "center"><i>Claiming FUELProxy token using the transaction token pattern</i></figcaption><br />

![FUELProxy mint flow](01-UpdateStrategy/FUELProxyUpdate.svg)

<figcaption align = "center"><i>Updating the list of valid FUELMintingPolicies</i></figcaption><br />

![FUELProxy burn flow](01-UpdateStrategy/FUELProxyBurn.svg)

<figcaption align = "center"><i>Burning FUELProxy token using the transaction token pattern</i></figcaption><br />

### 3.5 Governance of updates
In this section, we discuss options for governing updates. Previously, we
described the conditions for which an update happens are completely determined
by the script validator address which holds the `FUELOracleMintPolicy` or
`FUELOracleBurnPolicy` NFT.

Indeed, the only constraint that this script validator has is that it must have
`FUELOracleDatum` as its datum; so this means that the conditions on which this
validator may succeed (and hence permit an update) can be changed alongside an
update of the system. In other words, this mechanism allows updating of the
governance mechanism itself.

Alternatively, if we choose to also implement the versioning system described
below, reusing the same `VersionOracle` would make the design conceptually
simpler. Instead of having a concrete `FUELOracleMintPolicy` (or
`FUELOracleBurnPolicy`) and `FUELOracleValidator` pair, with slightly different
behaviour compared to the `VersionOraclePolicy`, we could just use the same
abstraction. This also allows us to reuse the same optimisations, such as
attaching reference scripts to VersionOracle utxos, thus also solving the
problem of storing old versions of scripts.

### 3.6 Sufficiency of implementing an update strategy for FUELMintingPolicy
In this section, we discuss why it is sufficient to provide a means to update
    the FUELMintingPolicy to update any of the other validators / minting
    policies in the system.

In the current implementation, we can classify the validators and minting policies into two categories:

- Validators and minting policies that do *not* interact in any way with the
  FUELMintingPolicy which is just the `CommitteeCandidateValidator`.

- Validators and minting policies that do interact in some way with the
  FUELMintingPolicy which is clearly everything else.

For the former of the two, we can upgrade these at any point since these are
used exclusively by the Bridge (which is completely offchain), so the Bridge
may arbitrarily decide that it would like to observe transactions from some
other validator address.

As for the latter case, in the view that `FUEL` is what all participants are
interested in; we may observe that, if any update of any minting policy or
validator occurs, then this implies that we would need to update the
FUELMintingPolicy since the FUELMintingPolicy is transitively parameterized by
all minting policies and validators of the latter case meaning an update to any
of the subparts would imply a change in the original FUELMintingPolicy.
So instead of providing a means to upgrade individual subparts of the system, it's
enough to just provide a method to upgrade the FUELMintingPolicy, and make the
new FUELMintingPolicy depend on the upgraded subparts instead.

For example, if we would like to upgrade the committee hash part of the system,
since the FUELMintingPolicy already depends on the committee hash, any change
the committee hash would require a change to the FUELMintingPolicy; hence we
only need to be concerned about updating the FUELMintingPolicy.

### 3.7 Completeness of AND and OR Boolean Expresions
In this section, we discuss some "completeness" results of AND and OR boolean
expressions in `FUELProxyPolicy`.

_Background._
To fix notation, let capital letters `A`, `B`, ..., etc. denote arbitrary
minting policies.
We'll begin by stating something that is essentially tautology:
whenever a minting policy `A` mints in a transaction, then we can be certain
that whatever conditions `A` needed to verify are true.
So, a Plutus script running in the same transaction can verify that if a
minting policy `A` mints, then all of the conditions `A` needed to verify are
true.

Certainly, a Plutus script running in the same transaction can use logical
connectives like AND or OR to verify that some combination of minting policies
are true.

For example, if in a transaction minting policies `A` and `B` mint, a Plutus
script running in the same transaction may verify
$$A \land B$$
to denote that the both the conditions to mint `A` and `B` were true in the
transaction that minted both tokens.

Similarly, if in a transaction either `A` or `B` mints, then a Plutus script
running in the same transaction may verify
$$A \lor B$$
to denote that either of the conditions to mint `A` or `B` were true in the
transaction that minted either of these tokens.

Unfortunately, our use of logical connectives regarding minting policies as
atomic formulas in a logical sentence only stretches so far.
In particular, a Plutus script in a transaction can never be certain that the
conditions to mint a token `A` are false.
In other words, the logical negation connective of a minting policy does not
have a clear interpretation for another Plutus script in a transaction since
when a minting policy `A` does not mint, then either:
- `A` has failed (in which case the entire transaction is invalid); or
- `A` was simply not included in the transaction (but may have indeed been true
  or false)
So, the nonexistence of `A` in a transaction is insufficient to conclude
anything about whether the conditions to mint `A` are true or false.

Hence, for Plutus scripts which mint only if some collection of minting
policies mint, we are thus only interested in composing these collection of
minting policies with logical connectives such as AND and OR.

_Relation to `FUELProxyPolicy`._
For the following discussion, we will only be concerned when `FUELProxyPolicy`
is minting (although the same reasoning applies to when `FUELProxyPolicy` is
burning).

To fix notation, for every version written as $i=1,\dots,n$ of the
`FUELOracleDatum` that is identified by `FUELOracleMintPolicy`, write the
corresponding `amountToken` of the `ProxiedTokens` as $A_{i,0}$ and write the
extra tokens as $A_{i,1},\dots,A_{i,m_i}$.

In other words, we have the following notation
```
1 -> ProxiedTokens
    { amountToken = A_{1,0}
    , extraTokens = [ A_{1,1}, A_{1,2},  ..., A_{1,m_1} ]
    }
2 -> ProxiedTokens
    { amountToken = A_{2,0}
    , extraTokens = [ A_{2,1}, A_{2,2},  ..., A_{2,m_2} ]
    }
...
n -> ProxiedTokens
    { amountToken = A_{n,0}
    , extraTokens = [ A_{n,1}, A_{n,2},  ..., A_{n,m_n} ]
    }

```

We recall that `FUELProxyPolicy` mints only if *any* of the of the
`ProxiedTokens` mints in the `FUELOracleDatum` identified by the
`FUELOracleMintPolicy`, and `ProxiedTokens` mints only if *all* of the minting
policies mints.
In other words, we may regard the conditions for when `FUELProxyPolicy` mints
with the following boolean expression.
$$(A_{1,0} \land A_{1,1} \land \dots \land A_{1,m_1}) \lor (A_{2,0} \land A_{2,1} \land \dots \land A_{2,m_2}) \lor \dots \lor (A_{n,0} \land A_{n,1} \land \dots \land A_{n,m_n}).$$
In other words, `FUELProxyPolicy` mints only if some ORs of ANDs of some
collection of minting policies mints.

_The completeness problem._
We are now ready to state the completeness problem.
Recall that Plutus scripts that mint only if some collection of minting
policies mint are primarily interested in boolean expressions with only AND and
OR logical connectives.

More formally, we say $X$ is a $\land-\lor$ boolean expression if
- $X$ is an atomic formula such as $A,B,\dots$. In our case, these are minting policies.

- If $Y$ and $Z$ are $\land-\lor$ boolean expressions, then $X = Y \land Z$ is
  a $\land-\lor$ boolean expression.

- If $Y$ and $Z$ are $\land-\lor$ boolean expressions, then $X = Y \lor Z$ is a
  $\land-\lor$ boolean expression.

where we use the usual logical meaning of $\land$ and $\lor$.

Recall that `FUELProxyPolicy` mints only if a $\land-\lor$ boolean expression of the form
$$(A_{1,0} \land A_{1,1} \land \dots \land A_{1,m_1}) \lor (A_{2,0} \land A_{2,1} \land \dots \land A_{2,m_2}) \lor \dots \lor (A_{n,0} \land A_{n,1} \land \dots \land A_{n,m_n})$$
is true i.e., ORs of ANDs of a atomic formulas.
We call such an $\land-\lor$ boolean expression to be in *$\land-\lor$ disjunctive normal form*.

Finally, here's the problem -- indeed, there are many $\land-\lor$ boolean
expression of which are not $\land-\lor$ disjunctive normal form and
hence cannot be represented with `FUELProxyPolicy`.
For example,
$$A \land (B \lor C)$$
is in $\land-\lor$ disjunctive normal form and hence
`FUELProxyPolicy` cannot mint only if this $\land-\lor$ boolean expression is
true.

Fortunately, in this case, we could rewrite this $\land-\lor$ boolean
expression to the equivalent boolean expression
$$(A \land  B) \lor (A \land C)$$
which may correspond to a `FUELOracleDatum` as follows.
```
1 -> ProxiedTokens
    { amountToken = A
    , extraTokens = [ B ]
    }
2 -> ProxiedTokens
    { amountToken = A
    , extraTokens = [ C ]
    }
```
and hence `FUELProxyPolicy` can mint only if that given $\land-\lor$ boolean
expression is true.

So in general, we have the following theorem

_Theorem._ Every $\land-\lor$ boolean expression can be written in a
logically equivalent $\land-\lor$ disjunctive normal form.

This is what is meant by *completeness of AND and OR Boolean Expresions*.

Thankfully, this theorem is true so we can rest assured that the design of
`FUELProxyPolicy` can mint only if an arbitrary $\land-\lor$ boolean expression
of a collection of minting polices mints as it can be rewritten to a logically
equivalent ORs of ANDs of a collection of minting policies.

We sketch a proof.

_Proof sketch._
Suppose $X$ is an arbitrary $\land-\lor$ boolean expression. We want to show that $X$ can be
written in as a logically equivalent $\land-\lor$ disjunctive normal form. We do this by induction on $X$.
For the base case, an atomic formula is clearly already in $\land-\lor$ disjunctive normal form.
Assuming that inductive hypothesis holds, we proceed to the inductive case,
which has two cases.
- If $X = A \land B$, then we may apply the inductive hypothesis to $A$ and $B$
  to get that $A = A_1 \lor A_2 \lor \dots \lor A_a$ and $B = B_1 \lor B_2 \lor \dots \lor B_b$ where $A_i,B_i$
  denotes arbitrary logical ANDs of atomic formulas.
  Note: $X$ is true iff $A \land B$ is true iff $A_i$ and $B_j$ are true
  for some $i,j$.
  Hence, with this reasoning, we may write an equivalent $\land-\lor$ disjunctive normal form as follows
  $$\bigvee_{i = 1,\dots,a } \bigvee_{j = 1,\dots,b} A_i \land A_j$$

- If $X = A \lor B$, then we may apply the inductive hypothesis to $A$ and $B$
  write in $\land-\lor$ disjunctive normal form, and we are immediately done as ORing
  two formulas in $\land-\lor$ disjunctive normal form is clearly also in
  $\land-\lor$ disjunctive normal form.

QED.

_Complexity._
While it is nice that every $\land-\lor$ boolean expression can be written in
$\land-\lor$ disjunctive normal form, the construction given in the theorem can
have terms that grow very large in size very quickly -- I believe in
$O(n^{2n})$. In practise, this hopefully won't be a problem as the versions /
modularity should be kept fairly small. Indeed, the conditions which two
minting policies mint can always be merged into a single minting policy to
reduce $\land-\lor$ boolean expression sizes.

### 3.8 Case Study: Onchain Modularity and Updates with the Committee Signing Scheme
This section discusses how one can achieve modularity and update the committee
signing scheme with the `FUELProxyPolicy`.

Let's suppose the scenario is as follows.

1. The sidechain is started with a committee signing scheme.

2. Many years later (with many unclaimed FUEL tokens left onchain), we decide
   that it's far better to replace the committee signing scheme.

3. New FUEL transactions are created with the new committee signing scheme.

We would like to: support users claiming their old unclaimed FUEL, and users to
be able to claim the new FUEL from the new committee signing scheme.

We first discuss some steps towards modularization of `FUELMintingPolicy` via
the `FUELProxyPolicy`.

Recall from the [main
specification](https://github.com/mlabs-haskell/trustless-sidechain/blob/master/docs/Specification.md)
that `FUELMintingPolicy` verifies the all of following conditions:

1. The FUEL claimed corresponds to an element in a merkle root that was
   inserted (i.e., minted by the `SignedMerkleRoot` policy) and hence must be
   parameterized by the currency symbol of the `SignedMerkleRoot` policy.
   Also, it verifies that the number of `FUEL` tokens minted this transaction
   corresponds to the number specified in the merkle root.

2. the hash of the transaction of the FUEL claimed has just been inserted in
   the distributed set and hence must be parameterized by the currency symbols
   of elements inserted in the distributed set.

Indeed, we could modularize the `FUELMintingPolicy` by splitting it into two
minting policies, say `FUELSignedMerkleRootPolicy` and `ClaimUniquePolicy`
which verify conditions 1. (with the modification that the transaction should
mint tokens with token name empty string instead of `FUEL`) and 2.
respectively.

So, the `FUELOracleDatum` uniquely identified the `FUELOracleMintPolicy` of a
`FUELProxyPolicy` could be written as
```
0 -> ProxiedTokens
    { amountToken = FUELSignedMerkleRootPolicy currency symbol
    , extraTokens =  [ ClaimUniquePolicy currency symbol ]
    }
```
so this `FUELProxyPolicy` would verify the same conditions as the original
`FUELMintingPolicy` did in the specification.

Now, we discuss how we can change the committee signature scheme while
supporting claiming old tokens. Recall from the main specification that the
`SignedMerkleRoot` policy creates a merkle root of FUEL transactions only if
the current committee (identified by an NFT, say `CommitteeNFT`) has signed the
merkle root and hence must be parameterized by a currency symbol of
`CommitteeNFT` to uniquely identify the current committee.

The first step towards changing the committee signing scheme would be to pay
`CommitteeNFT` to a new UTxO which contains the public keys of the new
signature scheme.

Note that if we change the committee signing scheme of `SignedMerkleRoot`,
this creates a distinct policy, say `SignedMerkleRoot'`, which has a distinct
currency symbol from `SignedMerkleRoot`.

So, if `FUELSignedMerkleRootPolicy` is parameterized by the new
`SignedMerkleRoot'` policy, we would get a new `FUELSignedMerkleRootPolicy` policy,
say `FUELSignedMerkleRootPolicy'`.

Thus, to support claiming of new FUEL tokens in the `FUELProxyPolicy`, we would
modify the `FUELOracleDatum` uniquely identified the `FUELOracleMintPolicy` of
a `FUELProxyPolicy` to as follows.
```
0 -> ProxiedTokens
    { amountToken = FUELSignedMerkleRootPolicy currency symbol
    , extraTokens =  [ ClaimUniquePolicy currency symbol ]
    }
1 -> ProxiedTokens
    { amountToken = FUELSignedMerkleRootPolicy' currency symbol
    , extraTokens =  [ ClaimUniquePolicy currency symbol ]
    }
```
Note that offchain we need to do some work to ensure that we give each
`ProxiedTokens` a unique integer. Indeed, one could hash the `ProxiedTokens` to
deterministically compute this.

In words: this new `FUELProxyPolicy` allows users to either:

- claim `FUEL` from merkle roots that were minted with the old
  `FUELSignedMerkleRootPolicy`; or

- claim `FUEL` from merkle roots that were minted with the new
  `FUELSignedMerkleRootPolicy'`.

## 4. Versioning Implementation
In this section we discuss how different versions will be maintained on-chain.
As mentioned in the previous sections, the implementation of the an on-chain
versioning system is not a requirement, but it has some benefits:
- backwards compatibility
- utilising reference scripts
- single source of truth for scripts
- modularity

#### Backwards compatibility
Without on-chain versioning, the user must use the version of the toolkit that
matches the version of the claim. With versioninig, the same off-chain code can
handle various versions. However, it's worth mentioning that maintaining backwards
compatibility could still be difficult on the off-chain code if the interface of
the policies/validators change.

#### Utilising reference scripts
Versioning design is tightly coupled with the use of reference scripts, which will
reduce the maintenance cost of the chain (e.g. FUEL mint fees [reduced by more than 50%](https://github.com/mlabs-haskell/trustless-sidechain/pull/359/files#diff-1aa9f7592eb75953f0ccffa0eb1b9029c9791cc5cdc799df278f32a4406b2bb5L195-R203)).

We can implement the same optimisations using reference scripts without versioning,
and the result would actually end up being similar to this versioning design (we
would need an oracle validator, where the reference scripts are stored in separate UTxOs).
However, without on-chain versioning the off-chain code has to verify, that the
reference script on-chain matches the off-chain version, and if not, it must fall
back to including the script in the transaction itself.

#### Single source of truth for scripts
With versioning, off-chain script handling becomes simpler:
- protocol initialisation and update sends the scripts to the `VersionOracle`
- all transactions use the `VersionOracle` to find the validator addresses and scripts

Without versioning, for all transactions the off-chain SDK includes the Plutus scripts
themselves (or we can use reference scripts, as explained above).

#### Modularity
Resulting from the single source of truth property, we can easily swap out certain scripts
in our protocol, even without a source code change, because there are no hard-coded refrences
in the scripts themselves. We could provide multiple implementation for a certain script
(e.g. different signature schemes for merkle root insertion and committee handover), and the
end-users would have the freedom, to choose from them.

### 4.1 Implementation

We implement a new validator and a new minting policy:

- `VersionOracleValidator`: validator address holding the references to all the above mentioned
  validators and minting policies in [#2](#2-strategies).
- `VersionOraclePolicy`: this token will prove that the version update was approved and the
  references are valid.

Both of the above are parameterised by the `GenesisUtxo`.
Also, we will modify `FUELMintingPolicy` and `MPTRootTokenMintingPolicy` to include the current
protocol version in their signed message and only allow minting with the actual version.

Furthermore, all scripts depenedending on some other script must use the VersionOracle to
get their dependencies (currency symbols or validator hashes) instead of a script parameter
or any other method to ensure that we get the above mentioned benefits. All script must use
the same version, to avoid unforeseable incompatibilities between script. To achieve this
all scripts must match their own version with the scripts of their dependencies.

![VersionOracle flow](01-UpdateStrategy/VersionOracle.svg)

<figcaption align = "center"><i>High level concept of versioning</i></figcaption><br />

The version oracle allows four actions: initialisation, insertion, update, invalidation.
All actions except initalisation require a specific signature from the governance mechanism.
Initialisation must occur together with the protocol initialisation, consuming the genesisUtxo.

**Message of the Update Signature:**

```haskell
data SignedVersionOracle
  = InsertVersionOracle VersionOracle ScriptHash
  -- ^ Adding a new version alongside the existing ones
  --    VersionOracle: identifier for the new version (scriptId + version)
  --    ScriptHash: hash of the new Plutus Script
  | UpdateVersionOracle VersionOracle ScriptHash Version
  -- ^ Replacing a specific version
  --    VersionOracle: identifier for the new version (scriptId + version)
  --    ScriptHash: hash of the new Plutus Script
  --    Version: old version to be invalidated (it will use the scriptId from VersionOracle)
  | InvalidateVersionOracle VersionOracle
  -- ^ Removing a version
  --    VersionOracle: identifier for the old version (scriptId + version)
```

#### 4.1.1. VersionOracleValidator

For each validator or minting policy, a separate UTxO with the following datum will
be created at the `VersionOracleValidator`. A `VersionOraclePolicy` token must be present with the
UTxO to prove its validity. Furthermore, each UTxO will also include a reference script
(see [CIP33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)), holding the actual validator or minting policy script.
This design allows multiple versions of the same validator.

**Datum:**

```haskell
data VersionOracle = VersionOracle
  { version :: Int
  -- ^ `version` of the protocol
  , scriptId :: Int
  -- ^ `scriptId` is the unique identifier of the validator
  }
```

Spending from the validator verifies:
 - a token was burnt (invalidation)

OR

 - exactly one VersionOracleToken exists in the transaction inputs
 - exactly one VersionOracleToken exists in the transaction outputs
 - `SignedVersionOracle` is built onchain using the `UpdateVersionOracle` constructor
 - `SignedVersionOracle` is signed by the governance mechanisms

#### 4.1.2. VersionOraclePolicy

This token will prove that the `VersionOracle` datum was approved by the committee.

**Mint/burning rules:**
 - `GenesisUtxo` is spent

OR

 - exactly one token exists in the transaction outputs (insertion) OR in the transaction inputs (invalidation)
 - `SignedVersionOracle` is built onchain using the `InsertVersionOracle` or `InvalidateVersionOracle` constructor respectively
 - `SignedVersionOracle` is signed by the governance mechanisms
