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
This section discusses in more detail how to apply the transaction token pattern to the FUELMintingPolicy.
In the development of these ideas, we will later introduce a new minting policy, `FUELMintingProxyPolicy`, whose tokens will be regarded as the FUEL tokens.

As a high level idea, the Transaction Token Pattern will change the
FUELMintingPolicy to forward all of its validations to some
*collection of minting policies* stored in the datum at a distinguished UTxO, and
the FUELMintingPolicy will mint only if the aforementioned collection of
minting policies *all* mint in the same transaction. The extra tokens minted from the aforementioned minting policies may be paid to any address and does not effect the functionality of the system[^extraTokensNote].

[^extraTokensNote]:
    The extra tokens could be sent directly back to the user's wallet, or paid to some arbitrary validator script. The former option makes this confusing for users, and the latter scenario will slightly increase the cost of the transaction (min UTxO fee).

Provided we have sufficient governance mechanisms, one may arbitrarily change
the distinguished UTxO which holds the collection of minting policies to modify
the sufficient conditions for the FUELMintingPolicy to mint. In essence, this
allows one to upgrade the FUELMintingPolicy without requiring changes to
existing FUEL already stored onchain i.e., there is no need to migrate existing
tokens over to a new upgraded FUELMintingPolicy which can be costly (in terms
of ada) if there are already many FUEL tokens existing onchain.

Moreover, since the FUELMintingPolicy will mint only if the collection of
minting policies *all* mint, this provides some modularity onchain with regards
to the sufficient conditions for the FUELMintingPolicy in the following sense:

- If an upgrade would like to modify an existing minting policy in the
  collection of minting policies, this amounts to replacing that minting policy
  with a new one.

- If an upgrade would like to add a minting policy to the collection of minting
  policies (in effect, this adds more required conditions to mint a FUEL
  token), then this amounts to adding a minting policy to the collection of
  minting policies in the UTxO.

One shortcoming of this would be an upgrade which modifies the
FUELMintingPolicy to mint only if *either* of two distinct minting policies
would mint (say, in the case of a partial version upgrade). This would require
replacing the entire collection of minting policies with a single minting
policy that mints only if either of the two distinct minting policies would
mint.
As an aside -- this suggests that it is sufficient for the collection of
minting policies to be of exactly size 1, but we will not pursue this idea
further.

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
tokens.

We will need a minting policy `FUELOracleMintingPolicy` to create an NFT (so this must
be parameterized by a UTxO) to act as an oracle which will uniquely identify
the UTxO that holds the collection of minting policies.

The collection of minting policies will reside at a script validator
address,`FUELOracle`, which has as datum
```
newtype FUELOracleDatum =
    { unFUELOracleDatum :: [CurrencySymbol]
    }
```
That is, this contains the collection of the currency symbols of minting
policies which are required for `FUELProxyPolicy` to mint. We will also
require that these currency symbols are lexicographically sorted (with the
exception of the first currency symbol which determines how much `FUELProxyPolicy`
to mint) to make onchain verifications easier.

It is outside the scope of this document to discuss the conditions for when
this validator will succeed, as there would need to be some sort of governance
mechanism which decides when we may upgrade `FUELProxyPolicy`. See
[3.5](#35-governance-of-updates) for details.

Finally, `FUELProxyPolicy` will be parameterized by the currency symbol of
the `FUELOracle` and will mint only if the following are satisfied:
- `FUELProxyPolicy` only has token name `FUEL`;
- there is a reference input in the current transaction which contains a
  `FUELOracleMintingPolicy` token with `FUELOracleDatum` as datum;
- the first currency symbol `c` in the `FUELOracleDatum` mints `k` tokens iff
  `FUELProxyPolicy` mints `k` tokens; and
- for every currency symbol `c` in the `FUELOracleDatum`, at least one such `c`
  is minted.

We summarize the entire workflow.

**Workflow**
1. An NFT `FUELOracleMintingPolicy` is minted, and paid to the `FUELOracle` validator script with datum
```
FUELOracleDatum {unFUELOracleDatum :: [ Currency symbol of FUELMintingPolicy ] }
```
2. Users may mint `FUELProxyPolicy` for `FUEL` where we note that we have
   `FUELProxyPolicy` minting only if `FUELMintingPolicy` mints.
3. A governance mechanism chooses to upgrade the system by spending the UTxO that holds the `FUELOracleMintingPolicy` NFT at validator `FUELOracle`, paying it to a `FUELOracle` validator address with a new `FUELOracleDatum` with new currency symbols in the datum.
4. Note that any new `FUEL` tokens must now validate with the new collection of
   minting policies provided.
5. Steps 2., 3., 4. may be repeated indefinitely for users and governance mechanisms.

![FUELProxy mint flow](01-UpdateStrategy/FUELProxyMint.svg)

<figcaption align = "center"><i>Claiming FUELProxy token using the transaction token pattern</i></figcaption><br />

![FUELProxy mint flow](01-UpdateStrategy/FUELProxyUpdate.svg)

<figcaption align = "center"><i>Updating the list of valid FUELMintingPolicies</i></figcaption><br />

_Burning `FUELProxyPolicy`_
Burning `FUELProxyPolicy` presents its own challenges. Unlike minting, where
one may arbitrarily mint tokens iff the minting policy succeeds, burning tokens
requires the user to additionally spend such tokens from an address. This is
inconvenient since the first currency symbol in `FUELOracleDatum` must be
present in some address when attempting to burn, but as time goes on updates
will invalidate such tokens.

So, instead of forcing users to have such tokens in their wallet to burn, we will
parameterize `FUELProxyPolicy` with an additional currency symbol, say
`burnCurrencySymbol`, that will serve the same purpose as `FUELOraclePolicy`
(i.e., `burnCurrencySymbol` is an NFT which uniquely identifies a UTxO with a
`FUELOracleDatum`), except that `FUELProxyPolicy` will *burn* `k` tokens only if
the `burnCurrencySymbol` *mints* `k` tokens in the same transaction.

Clearly, this removes the need to spend particular tokens when burning
`FUELProxyPolicy`.

![FUELProxy burn flow](01-UpdateStrategy/FUELProxyBurn.svg)

<figcaption align = "center"><i>Burning FUELProxy token using the transaction token pattern</i></figcaption><br />

### 3.5 Governance of updates
In this section, we discuss options for governing updates. Previously, we
described the conditions for which an update happens are completely determined
by the script validator address which holds the `FUELOracle` NFT.

Indeed, the only constraint that this script validator has is that it must have
`FUELOracleDatum` as its datum; so this means that the conditions on which this
validator may succeed (and hence permit an update) can be changed alongside an
update of the system. In other words, this mechanism allows updating of the
governance mechanism itself.

Alternatively, if we choose to also implement the versioning system described
below, reusing the same `VersionOracle` would make the design conceptually
simpler. Instead of having a concrete `FUELOraclePolicy` and
`FUELOracleValidator` pair, with slightly different behaviour compared to the
`VersionOraclePolicy`, we could just use the same abstraction. This also allows
us to reuse the same optimisations, such as attaching reference scripts to
VersionOracle utxos, thus also solving the problem of storing old versions of
scripts.

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
