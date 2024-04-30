# Delegation mechanism for Cardano Native Assets

This document describes the implementation of a staking and delegation
mechanism in `trustless-sidechain` for PoS Partner Chains whose stake distribution
is represented as Cardano Native Assets (CNA). Such a mechanism is required for
Minotaur+ (Theseus).

## 1. Purpose

Minotaur+ will support multi-resource consensus protocols where resources are
represented either as Ada, the currency of stake on the Cardano blockchain,
or as Cardano Native Assets, used as the unit of stake distribution on a PoS
Partner Chain.

`trustless-sidechain` provides features through which participants in a PoS
Partner Chain can delegate to block producers and remove delegations, with
the stake delegation data captured on the Cardano blockchain. Such a mechanism
is necessary to support the fundamental operations of a PoS blockchain. The
Cardano blockchain provides that functionality for delegations in Ada in a
way that is not extensible to delegations in CNA.

## 2. Design
The CNA delegation mechanism relies on two Plutus scripts:

* A **minting policy** to control minting and burning of `Minotaur Stake`
  tokens. The token's only purpose is to mark that PC native tokens
  controlled by a wallet are delegated. One token is minted to indicate
  delegation and one is burned to remove the delegation. This policy
  *does not* mint or burn the native token itself.
* A **validator** script whose address is the one to which the
  `Minotaur Stake` token is sent on delegation and from which the token
  is spent when removing a delegation delegation. The script verifies a
  UTxO containing the `Minotaur Stake` token marker is spent only if
  certain conditions hold guaranteeing the stake being removed is associated
  with the delegator address and stake pool identifier for which that delegation
  originally was made.

The minting policy and validator therefore provide witnesses on the Cardano
blockchain to the stake delegation distribution in the PC native token, while
also providing a means for native token holders to delegate to block producers
and in turn for block producers to reward delegators. Delegation and rewards are
managed by submitting transactions to the Cardano blockchain that are correct as
validated by those scripts.

### 2.1 Minting policy
A UTxO marks the delegation of a given wallet's native tokens to a stake pool
if it contains a `Minotaur Stake` token. A transaction minting a `Minotaur Stake`
token is considered valid if

* It is signed by the delegator's stake key public key hash,
* It mints exactly one `Minotaur Stake` token to mark the delegation,
* The transaction output holding the token is at the validator's address,
* The transaction output holding the token contains datum with
** A PC address to which rewards are sent,
** The public key hash of the delegator's stake key,
** The identifier for the stake pool to which the native tokens will be delegated, and
** The `Minotaur Stake` token's currency symbol.

Once a `Minotaur Stake` token is minted, the PC native tokens held at the
delegator's address are thereby considered delegated to the stake pool
indicated in the corresponding UTxO's datum. Stake pool operators then may
send rewards to the wallet address provided alongside the stake token.

PC native tokens are undelegated by spending the UTxO containing the
stake token and simultaneously burning that one token. A transaction
canceling a delegation is considered valid if it is signed by the
delegator's stake key public key hash and if it satisfies the constraints
the validator script imposes.

### 2.2 Validator script
<!-- TODO: Review this section once the validator logic is written -->
When a `Minotaur Stake` token is minted, it is sent to the address of the
validator script. The validator script's conditions must be met in order
to spend the UTxO containing the stake token and hence to cancel a delegation.

The script ensures a transaction attempting to spend the UTxO containing
the `Minotaur Stake` token satisfies the following:
* Exactly one `Minotaur Stake` token is burned,
* The UTxO's datum contains the correct delegator and stake pool
  identifiers
* The transaction is signed by the stake key public key of the delegator
  who minted the `Minotaur Stake` token originally.

### 2.3 Computing stake delegation distribution

To compute the stake delegation distribution in the PC native token, it is enough
to query for all UTxOs held at the validator's address. Each such UTxO should hold
exactly one `Minotaur Stake` token. Then, the amount of native tokens delegated to
a particular stake pool id can be computed by inspecting the datum of each of those UTxOs:
The quantity of tokens at addresses corresponding to each PubStakeKeyHash is summed.

You can use this SQL query on the `cardano-db-sync` PostgreSQL database.
`<MinotaurStakeValidator>` and `<MinotaurStakePolicy>` can be obtained using
`sidechain-main-cli addresses` command. `<STAKE_POOL_ID>` is the id of a stake pool
for which you want to calculate the total stake distribution. `<CNA_POLICY>` is a
Cardano Native Asset that is being staked. It is chosen by the partner chain that
wants to use CNA stake delegation mechanism.

```sql
select minotaur_delegation_datum_helper.value->'list'->1->>'bytes' as stakePubKeyHash,
       minotaur_delegation_datum_helper.value->'list'->0->>'bytes' as partnerChainRewardAddress,
       minotaur_delegation_datum_helper.value->'list'->2->>'bytes' as stakePoolId,
       Sum(stake_ma_tx_out.quantity) as stake_amount
from utxo_view as minotaur_delegation_utxo
join ma_tx_out as minotaur_delegation_ma_tx_out
on minotaur_delegation_ma_tx_out.tx_out_id = minotaur_delegation_utxo.id
join multi_asset as minotaur_delegation_multi_asset
on minotaur_delegation_multi_asset.id = minotaur_delegation_ma_tx_out.ident
join datum as minotaur_delegation_datum
on minotaur_delegation_datum.hash = minotaur_delegation_utxo.data_hash
JOIN (SELECT *,
             ROW_NUMBER() OVER (PARTITION BY (minotaur_delegation_datum.value->'list'->1->>'bytes') ORDER BY  minotaur_delegation_datum.tx_id DESC) AS rn
      from datum as minotaur_delegation_datum
      where minotaur_delegation_datum.value->'list'->1->>'bytes' is not null
) as minotaur_delegation_datum_helper
on minotaur_delegation_datum_helper.id = minotaur_delegation_datum.id
and minotaur_delegation_datum_helper.rn = 1
join stake_address as stake_address
on stake_address.hash_raw = decode('e0'||(minotaur_delegation_datum.value->'list'->1->>'bytes'), 'hex')
join utxo_view as staked_utxo
on staked_utxo.stake_address_id = stake_address.id
join ma_tx_out as stake_ma_tx_out
on stake_ma_tx_out.tx_out_id = staked_utxo.id
join multi_asset as stake_multi_asset
on stake_multi_asset.id = stake_ma_tx_out.ident
where minotaur_delegation_utxo.address = <MinotaurStakeValidator>
and minotaur_delegation_multi_asset.policy = decode(<MinotaurStakePolicy>, 'hex')
and minotaur_delegation_ma_tx_out.quantity > 0
and minotaur_delegation_datum.value->'list'->2->>'bytes' = <STAKE_POOL_ID>
and stake_multi_asset.policy = decode(<CNA_POLICY>, 'hex')
group by (minotaur_delegation_datum_helper.value)
```


### 2.4 Versioning
<!-- TODO: -->

## 3. CLI commands
<!-- TODO: update this section once the CLI commands are written -->

The `trustless-sidechain` CLI application provides two sub-commands related to
CNA delegation:

* `delegate-stake`, to delegate PC native tokens
* `cancel-delegate-stake` to remove a delegation.
* `get-own-minotaur-delegations` to get information about all delegations signed by the owner's stake pub key hash.
* `get-minotaur-delegations-for-stake-pool` to get information about all delegations to the given stake pool.

### 3.1 `delegate-stake`
```bash
sidechain-main-cli delegate-stake --spo-id <SPO_ID> --partner-chain-reward-address <PARTNER-CHAIN-REWARD-ADDRESS>
```

For example, to delegate native tokens held at wallet address to stake pool
"cadabra" with reward address "abcabc" you can do

```bash
sidechain-main-cli delegate-stake --spo-id "cadabra" --partner-chain-reward-address "abcabc"
```

### 3.2 `cancel-delegate-stake`
<!-- TODO: update this section once the `cancel-delegate-stake` command is written -->

### 3.3 `get-own-minotaur-delegations`
```bash
sidechain-main-cli get-own-minotaur-delegations
```

This command takes no extra arguments. The stake pub key hash for which the delegations are searched for is obtained during runtime by the CTL.

### 3.4 `get-minotaur-delegations-for-stake-pool`
```bash
sidechain-main-cli get-minotaur-delegations-for-stake-pool --spo-id <SPO-ID>
```

For example, to get all delegations made to the SPO "cadabra" you can do
```bash
sidechain-main-cli get-minotaur-delegations-for-stake-pool "cadabra"
```
