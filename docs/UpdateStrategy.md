# Update strategy

In this specification we describe the mechanisms of the Cardano mainchain part of the trustless
sidechain protocol update. To allow the protocol to evolve with time, we need to migrate from old
validators to new ones, deprecate old tokens to new ones.

We achieve the update by using an oracle validator. This validator address will be the single
source of truth, to find the validators and minting policies for the protocol.

## 1. Assumptions

- The governance strategy required to approve the protocol update is out of scope of this specification
-

## 2.Strategies

We considered two strategies, we call them full migration and versioned update:

- _migration strategy_: at any given point in time, there's only one valid version of a
  validator or minting policy. On an update event, all assets have to be migrated to the new
  protocol. The benefit of this approach is the simplicity, however the migration cost of a
  long-living chain can be tremendously costly.
- _versioned update_: in this strategy, we would maintain references to old validators and minting
  policies, giving time for the protocol users to migrate their assets as they like. With this
  strategy it is important to allow full migration, in case a version has to be abandoned due to
  a security issue. The update method must be flexible enough to allow addition and removal of
  certain validators/minting policies.

These strategies can be used in combination for optimal migration cost/complexity. In case of our
sidechain protocol, the following validators and minting policies will

- `FUELMintingPolicy`: versioned update
- `MPTRootTokenMintingPolicy`: versioned update
- `CommitteeCandidateValidator`: migration -
- `MPTRootTokenValidator`: versioned update
- `CommitteeHashValidator`: partial versioned update - new insertion is not allowed after the update
- `CommitteeHashPolicy`: partial versioned update - new mint is not allowed after the update
- `DsConfValidator`: partial versioned update -
- `DsConfPolicy`: partial versioned update
- `DsInsertValidator`: partial versioned update
- `DsKeyPolicy`: partial versioned update

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

For each version of the protocol, a separate UTxO with the following datum will be created at the
`VersionOracleValidator`. A `VersionOraclePolicy` token must be present with the UTxO to prove
its validity.

**Datum:**

```haskell
data VersionOracle = VersionOracle
  { version :: Int
  -- ^ `version` of the protocol
  , validators :: Map String ValidatorHash
  -- ^ `validators` is a mapping from validator name to validator hash
  , mintingPolicies :: Map String MintingPolicy
  -- ^ `mintingPolicies` is a mapping from minting policy name to validator hash
}
```

The same UTxO should also include the script itself, so users of the protocol can use reference
scripts ([CIP33](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0033)).

Spending from the validator requires all the `VersionOraclePolicy` tokens at the UTxO
to be burnt. This is discussed in more detail in [3.3. Invalidating a version](#33-invalidating-a-version)

### 3.2. VersionOraclePolicy

This token will prove that the `VersionOracle` datum was approved by the committee.

**Minting**:

- `versionHash` is signed by the committee (using the same multi-signature scheme
  as the sidechain certificate)
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
- migrate all tokens and datums from the old version (see [3.3. Migrating old tokens](#33-migrating-old-tokens))

The validator only allows spending any UTxOs, if the `VersionOracleValidator` token was burnt,
which in turn has to verify the signature of a special message.

### 3.3. Migrating old tokens

In case we are invalidating a version, we should provide a way to migrate old assets to their new
counterparts.

The following tokens would have to be migrated (burnt and reminted) in this scenario:

- `FUELMintingPolicy`
- `MPTRootTokenMintingPolicy`

Note that distributed set tokens and UTxOs are not mentioned, as they must be reminted with FUEL token.

TODO
We could have two strategies:

- allowing the new version of a minting policy to mint n amount of tokens in exchange for the old token.
-
