# Distributed Set
Following the requirements of the
[FUELMintingPolicy](./README.md#32-individual-claiming), we need a mechanism to
ensure that each individual claim can occur at most once. This document
outlines a method to achieve this by providing a currency symbol
[DsKeyPolicy](51-DsKeyPolicy) which mints iff a given `claimTransactionHash`
has never occurred before.

We describe the system by giving a graph theoretic definition of what we would
like to achieve, then discuss how to implement this in the block chain.

## 1. Basic Definitions
We want the following operations:

- a method to prove that a message digest (image of a hash function) has never
  been put in this set before; and
- a method to insert such a message digest into the set which should also only
  succeed if some other minting policy succeeds (in our case, the FUEL minting
  policy)

We realize this through an ordered linked list i.e., formally, given a set of
message digests; we have a connected directed graph with

- Nodes: message digests (we call these message digests a _key_)
- Edges: there is an edge between nodes `a` and `b` iff `a < b` (ordered
  lexicographically) and there does not exist a node `k` such that `a < k` and
  `k < b`.

In code, we represent this graph with the 'Node' type
```
data Node = Node
    { nKey :: BuiltinByteString
    , nEdge :: BuiltinByteString
    }
```
where `nKey` identifies the node, and `nEdge` represents an edge from this node
to some other node `nEdge`.

We say that a message digest _`str` is in the set_, iff there exists a node
`str`.

Since we are storing message digests (which are a fixed finite size), we know
that there exists a lower and upper bound to the message digests. In
particular, we are interested in storing message digests of blake2b_256 (which
are 32 bytes). Thus, it follows that

- `""` the empty string is a lower bound; and
- `replicate 33 '\255'` is an upper bound

of all message digests we are interested in storing.

This means that when we are storing message digests, we can represent an empty
set of 32 byte message digests as nodes `""` and `replicate 33 '\255'` as follows.
```
"" ---> replicate 33 '\255'
```
where it's easy to see that there are no message digests in the set. We call the node
```
Node
    { nKey = ""
    , nEdge = replicate 33 '\255'
    }
```
the _root node_.

_Claim._
Let `a` and `b` be nodes such that there exists an edge `a` to `b`.
Then, for every message digest `str` such that `a < str` and `str < b`,
`str` is not in the set.

_Sketch._ Immediately follows from defn. of an edge.

We reiterate: this claim asserts that to show that `str` is not already in the
set, it suffices to find a node `a` with an edge to `b` satisfying `a < str`
and `str < b`. Note that otherwise this is inconclusive.

## 2. Insertion Operation
To insert a string `str` (not already in the set) in the set, we need to find
the greatest lower bound of `str` of the keys already stored (think infimum).
Graphically, we find nodes `str'` and `str''` with an edge as follows.

```
... --> str' --> str'' --> ...
```

Note `str'` is unique with respect to `str` (this follows from the defn. of infimum), so what follows will be a
well defined mapping.

Then, to complete the insertion operation, we remove the edge `str'` to
`str''`, add the node `str`, and add edges `str'` to `str` and `str` to
`str''`. Graphically, we now have

```
... --> str' --> str --> str'' --> ...
```

It's easy to see that the invariants of the graph are still satisfied,
and by defn. `str` is now included in the set.

To summarize, we have the following results.

_Claim._
This mapping maintains the required invariants of the graph.

_Claim._
The node `str'` is unique with respect to `str`.

## 3. Implementation on the block chain
We discuss how we implement this in the block chain. We have the following
slogan.

_Slogan._
We have a graph `Node` iff we have a UTxO with `Node` stored at the UTxO.

The rest of this document is merely technicalities to ensure adversaries can't
cheat and prove that a node is not in the set when it already is.

As a high level idea of the validators and minting policies, we have 2 "types"
of validators / minting policies for: managing the static read-only
configuration of the distributed set, and managing the insertion of new nodes.

## 4. Implementation on Block Chain: Initialization

### 4.1. DsConfPolicy
This is a oneshot token to uniquely identify the UTxO holding the distributed
set configuration. It must be parametrized by a distinguished UTxO.

This minting policy verifies the following:
- It spends the distinguished UTxO, and exactly one of this token is minted (to
  ensure that this is a oneshot token)
- This token is paid to [DsConfValidator](42-DsConfValidator)

### 4.2. DsConfValidator
This is the validator which holds the configuration of the distributed set as
datum. It need not be parametrized by anything.

**Datum**
```
data DsConfDatum = DsConfDatum
  { dscKeyPolicy :: CurrencySymbol
    -- ^ minting policy for identifying nodes in the distributed set see
    -- [DsKeyPolicy](51-DsKeyPolicy)
  , dscFUELPolicy :: CurrencySymbol
    -- ^ minting policy for FUEL which [DsConfValidator](52-DsConfValidator) succeeds
    -- only if the FUEL minting policy succeeds
  }
```

This validator verifies the following:
- Always returns false and cannot be spent.

## 5. Implementation on Block Chain: Insertion

### 5.1. DsKeyPolicy
The keys of the distributed set are stored on-chain using the token name of the
minting policy `DsKeyPolicy`. It must be parameterized by the validator hash of
[`DsInsertValidator`](52-DsInsertValidator) and
[`DsConfPolicy`](41-DsConfPolicy). It uses the trivial redeemer, and verifies:

- It is spending exactly one `DsInsertValidator` with exactly one `DsKeyPolicy`
  token.

Or, it verifies all of the following:

- `DsConfPolicy` is being minted
- Exactly one `DsKeyPolicy` is being minted
- `DsKeyPolicy` is being paid to `DsConfValidator`
- The token name of this `DsKeyPolicy` is `nKey` of `rootNode`
- The `DsInsertValidator`'s datum is `DsDatum { dsNext = nEdge rootNode}` (see [5.2](#52-DsInsertValidator))

In more "plain English", we are either: in the former case where we are
inserting a new string, or in the latter case where we are initializing the
distributed set to just contain the root node.

_Remark._
The former case is loosely known as a _forwarding minting policy_, and has some
details
[here](https://github.com/Plutonomicon/plutonomicon/blob/main/forwarding1.md).

_Claim._ `DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTxO.
Moreover, if there is `DsKeyPolicy` sitting at a `DsInsertValidator` UTxO,
there is at most 1 `DsKeyPolicy` sitting at that UTxO.

_Proof._ Consider the first instance `DsKeyPolicy` is minted. This means that
the latter case must have succeeded, which means that `DsConfPolicy` must be
minted. Indeed, `DsConfPolicy` is a oneshot token, so the latter case can only
occur at most once. But in which case, this means that the `DsKeyPolicy` must
be paid to `DsConfValidator`.

As for the former case and the "moreover" claim, the correctness depends on
`DsInsertValidator` which we will see maintains the invariant that
`DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTxO.
QED.

We will also state a totally obvious fact with the aim of showing that for
every `DsKeyPolicy` mint (except the first mint), `DsKeyPolicy` succeeds iff
`DsInsertValidator` succeeds.

_Claim._ If `DsKeyPolicy` succeeds in the former case, then `DsInsertValidator`
succeeds.

### 5.2. DsInsertValidator
This validator does the heavy lifting / verification of the [insertion
operation](2-Insertion-Operation). It must be parameterized by the currency
symbol of `DsConfPolicy` which we recall uniquely identifies `DsConfValidator`.

**Datum:**
```
newtype DsDatum = DsDatum { dsNext :: BuiltinByteString }
```

It's important to note that we implement the `Node` data type (given
[1.](#1-basic-definitions)) by storing the `nKey` field in the token name of
the `DsKeyPolicy`, and storing the `nEdge` field in the `dsNext` field in
`DsDatum`.


This validator uses the trivial redeemer.

Suppose we want to insert the string `str`. Offchain, we must first find (as a
reference input) the input `DsConfValidator` uniquely identified by
`DsConfPolicy`. Then, we must find the `DsConfValidator` for which the token
name of the `DsKeyPolicy` (from the `DsConfValidator`), say `str'`, is the
greatest lower bound of `str`; and let `str''` denote the `dsNext` field in
this `DsDatum`[^1]. After these offchain computations to build the transaction,
we verify the following.

[^1]: Astute readers will notice that this would be a linear scan through every
  UTxO at the `DsConfValidator` address. As time goes on, this offchain
  computation would make the system cripplingly slow. Indeed, we should
  implement an efficient offchain query to optimize this later.

- The `FUELMintingPolicy` (stored in the datum of `DsConfValidator`) has minted
  a token.
- We mint exactly one `DsKeyPolicy` with token name as `str` i.e., the minted
  `DsKeyPolicy`'s token name is the string we wish to insert.
- There are exactly two outputs at the `DsInsertValidator` address, say `s` and
  `t`, for which `s` contains exactly one `DsKeyPolicy` token and similarly for
  `t`. Without loss of generality, let `s` denote the UTxO with a smaller token
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


We'll discuss some minor details here.


Graphically, the last 4 steps are verifying that as input to the transaction,
we have
```
               str''
... --> str' ------> ...
```
where `str'` is the token name of the `DsKeyPolicy` identifying the node, and
the `str''` above the arrow represents an edge from `str'` to `str''` which is
the `dsNext` field of `DsDatum`. Then, as output to the transaction, we have
```
               str          str''
... --> str' -------> str -------> ...
```

Indeed, this gives us the properties in [1.](1-Basic-Definitions) and
[2.](2-Insertion-Operation) which proves that `str` is not in the set, and we
are inserting `str` in the set in this transaction.

Most of the other verifications verify that we aren't "doing anything sneaky"
to create more nodes, or steal a token somewhere. Precisely, we have the
following claim (which continues the claim in [5.1](51-DsKeyPolicy))

_Claim._  Every `DsKeyPolicy` is sitting at a `DsInsertValidator`, and there is
at most one such `DsKeyPolicy` token at each `DsInsertValidator`.

Finally, we want to discuss one last obvious fact and its importance.

_Claim._ If `DsInsertValidator` succeeds, then `DsKeyPolicy` succeeds.

This immediately implies the final claim introduced in [5.1](51-DsKeyPolicy)
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
`FUELMintingPolicy` succeeds, then `DsKeyPolicy` succeeds[^3] (i.e., we are
inserting the `MerkleTreeEntry` for the first time -- preventing double mints).

[^3]: Technically, the _first_ successful 'DsKeyPolicy' disproves this claim,
  but the first successful 'DsKeyPolicy' does not correlate to hash of
  serialised `MerkleTreeEntry` because of the string length mismatch.

And clearly, we also have the converse, which prevents adversaries from denying
one's tokens (by inserting something in the distributed set before an honest
participant can mint their token). In other words, we have

_Claim._  `DsConfValidator` succeeds iff `FUELMintingPolicy` succeeds.

## 6. Future work for `DsConfPolicy` and `DsConfValidator`
Some readers may wonder why we need `DsConfPolicy` and `DsConfValidator`. This
is because we have "mutually dependent" validators and minting policies (e.g.
`DsKeyPolicy` and `DsInsertValidator`), and implementing this mutual dependence
cannot be implemented by parametrizing the scripts by each other since one
cannot compute this hash. Indeed,
[CIP-0069](https://github.com/cardano-foundation/CIPs/pull/321) provides a more
elegant solution but has not been completed yet.

## 7 Implementation on Block Chain: Workflow
We discuss the workflow. First, we must initialize the system with the
following step.
1. Mint `DsConfPolicy`, and pay this token to the `DsConfValidator` with the
   appropriately initialized `DsConfDatum`.

After, participants who wish to insert a string `str` should do the following steps.
1. Offchain, verify that `DsConfValidator` has the appropriately initialized
   `DsConfDatum` (i.e., the currency symbols are what we expect they should
   be). Without this check, we cannot be certain that the mutual dependence of
   minting policies and validators has been attained.
2. Mint a `DsKeyPolicy` with token name as `str` and build the transaction as
   discussed in [5.2.](52-DsInsertValidator).
3. Repeat 2. for every string one would like to insert.
