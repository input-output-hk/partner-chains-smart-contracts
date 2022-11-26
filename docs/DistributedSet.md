# Distributed Set

## 1. Basic Definitions
We briefly describe how this all works. We want the following operations:

- a method to prove that a message digest (image of a hash function) has never been put in this set before; and
- a method to insert such a message digest into the set which should also only succeed if some other minting policy succeeds (in our case, the FUEL minting policy)

We realize this through an ordered linked list i.e., formally, given a set
of message digests; we have a directed graph with

- Nodes: message digests (we call these message digests a _key_)
- Edges: there is an edge between nodes `a` and `b` iff `a < b` (ordered lexicographically) and there does not exist a node `k` such that `a < k` and `k < b`.

In code, we represent this graph with the 'Node' type
```
data Node = Node
    { nKey :: BuiltinByteString
    , nEdge :: BuiltinByteString
    }
```
where `nKey` identifies the node, and `nEdge` represents an edge from this node to some other node `nEdge`.

We say that a message digest _`str` is in the set_, iff there exists a node `str`.

Since we are storing message digests (which are a fixed finite size), we know that there exists a lower and upper bound to the message digests. In particular, we are interested in storing message
digests of blake2b_256 (which are 32 bytes) so

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

Sketch. Follows from the definition of an edge.

Why do we care about this? This claim asserts that to show that `str` is
not already in the set, it suffices to find a node `a` with an edge to `b`
satisfying `a < str` and `str < b`. Note that otherwise this is inconclusive.

## 2. Insertion Operation
To insert a string `str` (not already in the set) into the set, we need the
greatest lower bound of `str` of the keys already stored (think infimum) so
pictorially, we must have

```
str' ------> str''
```
where we necessarily have `str' < str < str''` from our assumptions.

Note `str'` and `str''` are unique with respect to `str`, so what follows will be a
well defined mapping.

Then, to complete the insertion operation, we transform this into

```
str' ---> str ---> str''

and it's easy to see that the invariants of the graph are still satisfied,
and by defn. `str` is now included in the set.
```

_Claim._
This mapping maintains the required invariants of the graph.

Sketch. Immediate.

_Claim._
The node `str'` and `str''` are unique with respect to `str`.

Sketch. By contradiction and lengthy case analysis.

## 3. Implementation on the block chain
We discuss how we implement this in the block chain. We have the following slogan.

_Slogan._
We have a graph `Node` iff we have a UTxO with `Node` stored at the UTxO.

The rest of this document is merely technicalities to ensure adversaries can't cheat and prove that a node is not in the set when it already is.

As a high level idea of the validators and minting policies, we have 2 "categories"
of validators / minting policies: for managing the static read-only
configuration of the distributed set, and for managing the insertion of new nodes.

## 4. Implementation on Block Chain: Initialization

### 4.1. DsConfPolicy
This is a oneshot token to uniquely identify the UTxO holding the distributed set configuration.
It must be parametrized by a distinguished UTxO.

This minting policy verifies the following:
- Spends a distinguished UTxO, and exactly one of this token is minted (to ensure that this is a oneshot token)
- This token is paid to [42-DsConfValidator](DsConfValidator)

### 4.2. DsConfValidator
This is the validator which holds the configuration of the distributed set as datum.

**Datum**
```
data DsConfDatum = DsConfDatum
  { dscKeyPolicy :: CurrencySymbol
    -- ^ minting policy for identifying nodes in the distributed set see TODO
    -- PUT A LINK HERE
  , dscFUELPolicy :: CurrencySymbol
    -- ^ minting policy for FUEL which DsConfValidator TODO PUT A LINK HERE
    -- succeeds only if the FUEL minting policy succeeds
  }
```

This validator verifies the following:
- Always returns false and cannot be spent.

## 5. Implementation on Block Chain: Insertion

### 5.1. DsKeyPolicy
The keys of the distributed set are stored on-chain using the `tokenName` of the minting policy `DsKeyPolicy`.
It must be parameterized by the validator hash of `DsInsertValidator` TODO LINK ME and `DsConfPolicy` TODO LINK ME.
It uses the trivial redeemer, and verifies either of the following:
- `DsConfPolicy` is being minted
- Exactly one `DsKeyPolicy` is being minted
- `DsKeyPolicy` is being paid to `DsConfValidator`
- The `tokenName` of this `DsKeyPolicy` is `nKey` of `rootNode`
- The `DsInsertValidator`'s datum is `DsDatum { dsNext = nEdge rootNode}` (see [5.2](#52-DsInsertValidator))
or
- It is spending exactly one `DsInsertValidator` with exactly one `DsKeyPolicy` token.

In more "plain English", we are either: in the former case where we are initializing the distributed set to just contain the root node, or in the latter case where we inserting a new string.

_Claim._ `DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTxO. Moreover, if there is `DsKeyPolicy` sitting at a `DsInsertValidator` UTxO, there is at most 1 `DsKeyPolicy` sitting at the UTxO.

_Proof._ Consider the first instance `DsKeyPolicy` is minted. This means that
the former case must have succeeded, which means that `DsConfPolicy` must be minted.
Indeed, `DsConfPolicy` is a oneshot token, so the former case can only occur at most once.
But in which case, this means that the `DsKeyPolicy` must be paid to `DsConfValidator`.

Now, for the latter case (and the "moreover" claim), the correctness depends on the `DsInsertValidator` which we will see maintains the invariant that `DsKeyPolicy` will always be sitting at a `DsInsertValidator` UTxO.

We will also state a totally obvious fact with the aim of showing that for every `DsKeyPolicy` mint (except the first mint), `DsKeyPolicy` succeeds iff `DsInsertValidator` succeeds.
_Claim._ If `DsKeyPolicy` succeeds in the latter case, then `DsInsertValidator` succeeds.

_Remark._
The latter case is loosely known as a _forwarding minting policy_, and has some details [here](https://github.com/Plutonomicon/plutonomicon/blob/main/forwarding1.md).

### 5.2. DsInsertValidator
This validator does the heavy lifting / verification of an insertion.
It must be parameterized by the oneshot token `DsConfPolicy` which we may recall uniquely identifies `DsConfValidator`.

**Datum:**
```
newtype DsDatum = DsDatum { dsNext :: BuiltinByteString }
```

It's important to note that we realize the `Node` data type (given [1.](#1-basic-definitions))
by storing `nKey` field in tokenName of the `DsKeyPolicy`, and storing the `nEdge` field in the `dsNext` field in `DsDatum`.
a

This validator uses the trivial redeemer. We discuss the verifications of this validator.
We must first find (as a reference input) the input uniquely identified by `DsConfPolicy`.
Then, let `key` denote the `tokenName` of the `DsKeyPolicy` (from the `DsConfValidator`) sitting at this validator, and
`key'` denote the `dsNext` field in this `DsDatum`. We verify the following.
- The `FUELMintingPolicy` (stored in `DsConfValidator`) has minted a token.
- We mint exactly one `DsKeyPolicy` with `tokenName` denoted as `inserted`.
- There are exactly two outputs at the `DsInsertValidator` address, say `s` and `t`, for
  which `s` contains exactly one `DsKeyPolicy` token and similarly for `t`.
  Without loss of generality, let `s` denote the UTxO with a smaller
  `tokenName` in `DsKeyPolicy`.
- The `tokenName` of `DsKeyPolicy` in `s` is `key`.
- The datum of `s` is has the field `dsNext` as `inserted`.
- The `tokenName` of `DsKeyPolicy` in `t` is `inserted`.
- The datum of `t` is has the field `dsNext` as `key'`.

We'll discuss some minor details here.


Pictorially, the last 4 steps are verifying that as input, we have
```
     key'
key ------>
```
where `key` denotes the node in the graph, and `key'` above the arrow denotes the edge from `key` to `key'`
becomes the outputs
```
     inserted             key'
key ----------> inserted ------->
```
where `key` again denotes the node in the graph, `inserted` above the arrow denotes an edge from `key` to `inserted`, and `key'` above the arrow denotes an edge from `inserted` to `key`.

Indeed, this gives us the properties in [1](1-Basic-Definitions) and [2](2-Insertion-Operation) to prove that `inserted` is not in the set, and we are inserting `inserted` in the set in this transaction.

Most of the other verifications verify that we aren't "doing anything sneaky" to create more nodes, or steal a token somewhere.
Precisely, we have the following claim (which continues the claim in [5.1](51-DsKeyPolicy))

_Claim._  Every `DsKeyPolicy` is sitting at a `DsInsertValidator`, and there is at most one such `DsKeyPolicy` token at each `DsInsertValidator`.

Finally, we want to discuss one last obvious fact and its importance.
_Claim._ If `DsInsertValidator` succeeds, then `DsKeyPolicy` succeeds.

This immediately implies the claim introduced in [5.1](51-DsKeyPolicy) which was as follows

_Claim._ For every `DsKeyPolicy` mint (except the first mint), `DsKeyPolicy` succeeds iff `DsInsertValidator` succeeds.

The importance of this claim is as follows -- by just looking at the `tokenName` of a `DsKeyPolicy` that was minted in a transaction, this is enough to conclude that: `tokenName` is not already in the set, and `tokenName` is being included in the set in this transaction. So, this means that for `FUELMintingPolicy` to verify that a `MerkleTreeEntry` has never been minted before, and is being minted for the first time ever; it amounts to verifying that the hash of the serialised `MerkleTreeEntry` is the `tokenName` of `DsKeyPolicy` being minted in the same transaction. This verification establishes that if the `FUELMintingPolicy` succeeds, then `DsKeyPolicy` succeeds[^fueliffdskey] (i.e., we are inserting the `MerkleTreeEntry` for the first time -- preventing double mints).

[^fueliffdskey] Technically, the _first_ successful 'DsKeyPolicy' disproves this claim, but the first successful 'DsKeyPolicy' does not correlate to hash of serialised `MerkleTreeEntry` because of the string length mismatch.

And clearly, we also have the converse, which prevents adversaries from denying one's tokens. In other words, we have

_Claim._  `DsConfValidator` succeeds iff `FUELMintingPolicy` succeeds.

## 6. Implementation on Block Chain: Workflow

TODO: Mention how this was meant to be used (the workflow)
----
The workflow to initialise the distributed set follows almost immediately from
the onchain verfications of `DsConfPolicy`. We spend a distinguished UTxO to
mint `DsConfPolicy`, pay `DsConfPolicy` to the validator script
`DsConfValidator` (for which `DsConfDatum` is initialized with the desired minting policies).
----
