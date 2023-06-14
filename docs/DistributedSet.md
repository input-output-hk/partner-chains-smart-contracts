# Distributed Set
Following the requirements of the
[FUELMintingPolicy](./README.md#32-individual-claiming), it is necessary to
ensure that each individual claim can occur at most once. This document
outlines a method to achieve this by providing a currency symbol
[DsKeyPolicy](#51-DsKeyPolicy) that mints if and only if a given `claimTransactionHash`
has never occurred before.

This description of the system gives a theoretical definition of requirements, then discusses how to implement this in the block chain.

## 1. Basic Definitions
The following operations are required:

- a method to prove that a message digest (image of a hash function) has never
  been put in this set before; and
- a method to insert such a message digest into the set which should also only
  succeed if some other minting policy succeeds (in this case, the FUEL minting
  policy)

An ordered linked list achieves this; formally, given a set of
message digests; there is a connected directed graph with

- Nodes: message digests (called a _key_)
- Edges: there is an edge between nodes `a` and `b` if and only if `a < b` (ordered
  lexicographically) and there does not exist a node `k` such that `a < k` and
  `k < b`.

This graph is represented in code with the `Node` type
```
data Node = Node
    { nKey :: BuiltinByteString
    , nEdge :: BuiltinByteString
    }
```
where `nKey` identifies the node, and `nEdge` represents an edge from this node
to some other node `nEdge`.

A message digest _`str` is in the set_, if and only if there exists a node
`str`.

Since message digests are being stored (which are a fixed finite size), there must exist a lower and upper bound to the message digests. The message digests of `blake2b_256`
(which are 32 bytes) are to be stored. Thus, it follows that

- `""` the empty string is a lower bound; and
- `replicate 33 '\255'` is an upper bound

of all message digests to be stored.

This means that when message digests are stored, an empty
set of 32 byte message digests can be represented as nodes `""` and `replicate 33 '\255'` as follows.
```
"" ---> replicate 33 '\255'
```
where it's easy to see that there are no message digests in the set. The node
```
Node
    { nKey = ""
    , nEdge = replicate 33 '\255'
    }
```
is called the _root node_.

_Claim._
Let `a` and `b` be nodes such that there exists an edge `a` to `b`.
Then, for every message digest `str` such that `a < str` and `str < b`,
`str` is not in the set.

_Sketch._ Immediately follows from defn. of an edge.

To reiterate: this claim asserts that to show that `str` is not already in the
set, it suffices to find a node `a` with an edge to `b` satisfying `a < str`
and `str < b`. Note that otherwise this is inconclusive.

## 2. Insertion Operation
To insert a string `str` (not already in the set) in the set, it is necessary to find
the greatest lower bound of `str` of the keys already stored (think [infimum](https://www.collinsdictionary.com/us/dictionary/english/infimum)).
Graphically, nodes `str'` and `str''` with an edge can be found as follows.

```
... --> str' --> str'' --> ...
```

Note `str'` is unique with respect to `str` (this follows from the definition of infimum), so what follows will be a
well defined mapping.

Then, to complete the insertion operation, remove the edge `str'` to
`str''`, add the node `str`, and add edges `str'` to `str` and `str` to
`str''`. This is shown graphically below:

```
... --> str' --> str --> str'' --> ...
```

It's easy to see that the invariants of the graph are still satisfied,
and by definition `str` is now included in the set.

To summarize, the results are as follows.

_Claim._
This mapping maintains the required invariants of the graph.

_Claim._
The node `str'` is unique with respect to `str`.

## 3. Implementation on the block chain
To implement this in the block chain, consider the following
slogan.

_Slogan._
There is a graph `Node` iff  there is a UTXO with `Node` stored at the UTXO.

The rest of this document is merely technicalities to ensure adversaries can't
cheat and prove that a node is not in the set when it already is.

As a high level idea of the validators and minting policies, there are two "types"
of validators / minting policies for: managing the static read-only
configuration of the distributed set, and managing the insertion of new nodes.

## 4. Implementation on Block Chain: Initialization

### 4.1. DsConfPolicy
This is a oneshot token to uniquely identify the UTXO holding the distributed
set configuration. It must be parameterized by a distinguished UTXO.

This minting policy verifies the following:
- It spends the distinguished UTXO, and exactly one of this token is minted (to
  ensure that this is a oneshot token)
- This token is paid to [DsConfValidator](#42-DsConfValidator)

### 4.2. DsConfValidator
This is the validator which holds the configuration of the distributed set as
datum. It need not be parametrized by anything.

**Datum**
```
data DsConfDatum = DsConfDatum
  { dscKeyPolicy :: CurrencySymbol
    -- ^ minting policy for identifying nodes in the distributed set see
    -- [DsKeyPolicy](#51-DsKeyPolicy)
  , dscFUELPolicy :: CurrencySymbol
    -- ^ minting policy for FUEL which [DsConfValidator](#52-DsConfValidator) succeeds
    -- only if the FUEL minting policy succeeds
  }
```

This validator verifies the following:
- Always returns false and cannot be spent.

## 5. Implementation on Block Chain: Insertion

### 5.1. DsKeyPolicy
The keys of the distributed set are stored on-chain using the token name of the
minting policy `DsKeyPolicy`. It must be parameterized by the validator hash of
[`DsInsertValidator`](#52-DsInsertValidator) and
[`DsConfPolicy`](#41-DsConfPolicy). It uses the trivial redeemer, and verifies:

- It is spending exactly one `DsInsertValidator` with exactly one `DsKeyPolicy`
  token.

Or, it verifies all of the following:

- `DsConfPolicy` is being minted
- Exactly one `DsKeyPolicy` is being minted
- `DsKeyPolicy` is being paid to `DsConfValidator`
- The token name of this `DsKeyPolicy` is `nKey` of `rootNode`
- The `DsInsertValidator`'s datum is `DsDatum { dsNext = nEdge rootNode}` (see [5.2](#52-DsInsertValidator))

In more "plain English", there are two cases: in the former case inserting a new string, or in the latter case initializing the
distributed set to just contain the root node.

_Remark._
The former case is loosely known as a _forwarding minting policy_, and has some
details
[here](https://github.com/Plutonomicon/plutonomicon/blob/main/forwarding1.md).

_Claim._ `DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTXO.
Moreover, if there is `DsKeyPolicy` sitting at a `DsInsertValidator` UTXO,
there is at most 1 `DsKeyPolicy` sitting at that UTXO.

_Proof._ Consider the first instance `DsKeyPolicy` is minted. This means that
the latter case must have succeeded, which means that `DsConfPolicy` must be
minted. Indeed, `DsConfPolicy` is a oneshot token, so the latter case can only
occur at most once. But in which case, this means that the `DsKeyPolicy` must
be paid to `DsConfValidator`.

As for the former case and the "moreover" claim, the correctness depends on
`DsInsertValidator` which maintains the invariant that
`DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTXO.
QED.

The following fact shows that for
every `DsKeyPolicy` mint (except the first mint), `DsKeyPolicy` succeeds iff
`DsInsertValidator` succeeds.

_Claim._ If `DsKeyPolicy` succeeds in the former case, then `DsInsertValidator`
succeeds.

### 5.2. DsInsertValidator
This validator does the heavy lifting / verification of the [insertion
operation](#2-Insertion-Operation). It must be parameterized by the currency
symbol of `DsConfPolicy` which uniquely identifies `DsConfValidator`.

**Datum:**
```
newtype DsDatum = DsDatum { dsNext :: BuiltinByteString }
```

It's important to note that the `Node` data type (given
[1.](#1-basic-definitions)) is implemented by storing the `nKey` field in the token name of
the `DsKeyPolicy`, and storing the `nEdge` field in the `dsNext` field in
`DsDatum`.


This validator uses the trivial redeemer.

Consider inserting the string `str`. Offchain, it is necessary to first find (as a
reference input) the input `DsConfValidator` uniquely identified by
`DsConfPolicy`. Then it is necessary to find the `DsConfValidator` for which the token
name of the `DsKeyPolicy` (from the `DsConfValidator`), say `str'`, is the
greatest lower bound of `str`; and let `str''` denote the `dsNext` field in
this `DsDatum`[^1]. After these offchain computations to build the transaction,
the onchain code verifies the following.

[^1]: Astute readers will notice that this would be a linear scan through every
  UTXO at the `DsConfValidator` address. As time goes on, this offchain
  computation would make the system cripplingly slow. There is a need to
  implement an efficient offchain query to optimize this later.

- The `FUELMintingPolicy` (stored in the datum of `DsConfValidator`) has minted
  a token.
- Exactly one `DsKeyPolicy` is minted with token name as `str` ie, the minted
  `DsKeyPolicy`'s token name is the string to insert.
- There are exactly two outputs at the `DsInsertValidator` address, say `s` and
  `t`, for which `s` contains exactly one `DsKeyPolicy` token and similarly for
  `t`. Without loss of generality, let `s` denote the UTXO with a smaller token
  name in `DsKeyPolicy`.
- The outputs `s` and `t` are "relatively small"[^2]
- The token name of `DsKeyPolicy` in `s` is `str'`.
- The datum of `s` is has the field `dsNext` as `str`.
- The token name of `DsKeyPolicy` in `t` is `str`.
- The datum of `t` is has the field `dsNext` as `str''`.


[^2]: This condition is a minor technicality where an adversary can pay a large
  amount of tokens to the outputs `s` and `t`, so the next time an honest
  participant attempts to spend either `s` or `t`, the output cannot be spent
  because the `ExUnits` are all spent trying to decode the `ScriptContext` and
  hence goes over budget. Interestingly, inspecting the code in
  [Plutus.Contract.StateMachine](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/StateMachine/OnChain.hs)
  it appears that this problem exists there as well (but was never resolved).


Some minor details are discussed here.


Graphically, the last 4 steps are verifying that as input to the transaction,
like this:
```
               str''
... --> str' ------> ...
```
where `str'` is the token name of the `DsKeyPolicy` identifying the node, and
the `str''` above the arrow represents an edge from `str'` to `str''` which is
the `dsNext` field of `DsDatum`. Then, as output to the transaction, there is
```
               str          str''
... --> str' -------> str -------> ...
```

Indeed, this gives the properties in [1.](#1-Basic-Definitions) and
[2.](#2-Insertion-Operation) which proves that `str` is not in the set, and `str` is inserted in the set in this transaction.

Most of the other verifications verify that there is no underhand process
to create more nodes, or steal a token somewhere. Precisely, the
following claim (which continues the claim in [5.1](#51-DsKeyPolicy)) can be made:

_Claim._  Every `DsKeyPolicy` is sitting at a `DsInsertValidator`, and there is
at most one such `DsKeyPolicy` token at each `DsInsertValidator`.

Finally, there is one last obvious fact, and its importance, to discuss.

_Claim._ If `DsInsertValidator` succeeds, then `DsKeyPolicy` succeeds.

This immediately implies the final claim introduced in [5.1](#51-DsKeyPolicy)
which was as follows

_Claim._ For every `DsKeyPolicy` mint (except the first mint), `DsKeyPolicy`
succeeds iff `DsInsertValidator` succeeds.

The importance of this claim is as follows -- by just looking at the token name
of a `DsKeyPolicy` that was minted in a transaction, this is enough to conclude
that: token name is not already in the set, and token name is being included in
the set in this transaction. So, this means that for `FUELMintingPolicy` to
verify that a `MerkleTreeEntry` has never been minted before, and is being
minted for the first time ever; it amounts to verifying that the hash of the
serialised `MerkleTreeEntry` is the token name of `DsKeyPolicy` being minted in
the same transaction. This verification establishes that if the
`FUELMintingPolicy` succeeds, then `DsKeyPolicy` succeeds[^3] (ie, inserting the `MerkleTreeEntry` for the first time -- preventing double mints).

[^3]: Technically, the _first_ successful 'DsKeyPolicy' disproves this claim,
  but the first successful 'DsKeyPolicy' does not correlate to hash of
  serialised `MerkleTreeEntry` because of the string length mismatch.

And clearly, there is also the converse, which prevents adversaries from denying
one's tokens (by inserting something in the distributed set before an honest
participant can mint their token). In other words, there is

_Claim._  `DsConfValidator` succeeds iff `FUELMintingPolicy` succeeds.

## 6. Future work for `DsConfPolicy` and `DsConfValidator`
Some readers may wonder why both `DsConfPolicy` and `DsConfValidator` are required. This
is because there are "mutually dependent" validators and minting policies (eg
`DsKeyPolicy` and `DsInsertValidator`), and implementing this mutual dependence
cannot be implemented by parameterizing the scripts by each other since one
cannot compute this hash. Indeed,
[CIP-0069](https://github.com/cardano-foundation/CIPs/pull/321) provides a more
elegant solution but has not been completed yet.

## 7 Implementation on Block Chain: Workflow
First, initialize the system with the
following step.
1. Mint `DsConfPolicy`, and pay this token to the `DsConfValidator` with the
   appropriately initialized `DsConfDatum`.

After, participants who wish to insert a string `str` should do the following steps.
1. Offchain, verify that `DsConfValidator` has the appropriately initialized
   `DsConfDatum` (ie, the currency symbols are what they should
   be). Without this check, there is no certainty that the mutual dependence of
   minting policies and validators has been attained.
2. Mint a `DsKeyPolicy` with token name as `str` and build the transaction as
   discussed in [5.2.](#52-DsInsertValidator).
3. Repeat 2. for every string one would like to insert.
