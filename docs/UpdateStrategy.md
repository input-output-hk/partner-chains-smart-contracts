# Update strategy - PROPOSAL

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

  The drawback is that this would slightly raise the fees due to the cost of an extra token minted for each transaction.

These strategies can be used in combination for optimal migration cost/complexity.
We also have to decide for each validator, wheter

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

## 3. Implementation:

We implement a new validator and a new minting policy:

- `VersionOracleValidator`: validator address holding the references to all the above mentioned
  validators and minting policies. Using reference scripts, we could also store the actual
  scripts themselves.
- `VersionOraclePolicy`: this token will prove that the version update was approved and the
  references are valid.

Both of the above are parameterised by the `SidechainParams`.
Also, we will modify `FUELMintingPolicy` and `MPTRootTokenMintingPolicy` to include the current
protocol version in their signed message and only allow minting with the actual version.

### 3.1. VersionOracleValidator

For each validator or minting policy, a separate UTxO with the following datum will
be created at the `VersionOracleValidator`. A `VersionOraclePolicy` token must be present with the
UTxO to prove its validity. This design allows multiple versions of the same validator.

**Datum:**

```haskell
data VersionOracle = VersionOracle
  { version :: Int
  -- ^ `version` of the protocol
  , scriptId :: Int
  -- ^ `scriptId` is the unique identifier of the validator
  , scriptHash :: ScriptHash
  -- ^ `validators` is the validator hash of the validator/minting policy
}
```

The same UTxO should also include the script itself, so users of the protocol can use reference
scripts ([CIP33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)).

Spending from the validator requires all the `VersionOraclePolicy` tokens at the UTxO
to be burnt. This is discussed in more detail in [3.3. Invalidating a version](#33-invalidating-a-version)

### 3.2. VersionOraclePolicy

This token will prove that the `VersionOracle` datum was approved by the committee.

**Minting**:

- `versionHash` is signed by the governance mechanism
- `versionHash` is stored as the tokenName of the token. (This will make it sure that the version
  datum isn't altered after minting)

```
versionHash = blake2b(cbor(VersionOracle))
```

**Burning**:
see [3.3. Invalidating a version](#33-invalidating-a-version)

- `concat("invalidate", versionHash)` message is signed by the committee

### 3.3. Invalidating a version

Invalidating a version will require us to

- burn the `VersionOraclePolicy` token
- remove the UTxO from the `VersionOracleValidator` (strictly speaking this is not necessary, but it will
  make the protocol simpler).

The validator only allows spending any UTxOs, if the `VersionOracleValidator` token was burnt,
which in turn has to verify the signature of a special message.
