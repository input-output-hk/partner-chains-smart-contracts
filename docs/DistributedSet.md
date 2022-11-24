# Distributed Set

We briefly describe how this all works. We want the following operations:

- A method to prove that a message digest (image of a hash function)
  has never been put in this set before, and
- A method to insert such a message digest into the set.

We realize this through an ordered linked list i.e., formally, given a set
of message digests; we have a directed graph with

- Nodes: message digests (we call these message digests a /key/)
- Edges: there is an edge between nodes `a` and `b` iff `a < b` and there does not exist a node `k` such that `a < k` and `k < b`.

In code, we represent this graph with the 'Node' type, where 'nKey'
identifies the node and the edge is represented with 'nEdge'.

We say that a message digest `str` is in the set, iff there exists a node
`str`.

_Claim._
Let `a` and `b` be nodes s.t. there exists an edge `a` to `b`.
Then, for every message digest `str` s.t. `a < str` and `str < b`,
`str` is not in the set.

Sketch. Follows from the definition of an edge.

Why do we care about this? This claim asserts that to show that `str` is
not already in the set, it suffices to find a node `a` with an edge to `b`
satisfying `a < str` and `str < b`. Note that otherwise this is inconclusive.

Also, since we are storing message digests (which are a fixed size and
also finite in size), we know that there exists a lower and upper bound to
the message digests. In particular, we are interested in storing message
digests of blake2b_256 so

- `""` the empty string is a lower bound
- `replicate 33 '\255'` is an upper bound

This means that we can have a root node as given in 'rootNode'.

To insert a string `str` (not already in the set) into the set, we need the
greatest lower bound of `str` in the set (think infimum) so pictorially, we
have

```
str' ------> str''

where we necessarily have `str' < str < str''` from our assumptions.
Note that the place we wish to insert is unique, so what follows will be a
well defined mapping.
```

Then, we transform this into

```
str' ---> str ---> str''

and it's easy to see that the invariants of the graph are still satisfied,
and by defn. `str` is included in the set.
```

_Claim._
This mapping maintains the required invariants of the graph.

_Claim._
There is a unique node for an insertion of a string not already in
the set.

And really, that's it.. this isn't a super complicated way to do this....
and probably this has made it overly complicated.

Now, we discuss how we actually do this in the block chain.. lovely.. We
store the message digest which identifies the node in a token name of a
utxo, and we represent the edge (to another node) via storing the token
name which identifies the other node in the datum of the utxo. It's really
just that easy.

Also, I should mention that we have some complexities with the validator
succeeding iff the minting policy (which identifies the node -- see
`mkDsKeyPolicy`). To achieve this, we store (at another utxo) a
`DsConfDatum` which stores the minting policy hash so that the validator
can validate iff and the minting policy succeeds; and the minting policy is
parameterized by the validator hash.

There's a bit of an annoyance with "how do I initialize this thing," but
it's easily resolved by minting an initial NFT (which pays to a utxo with
'DsConfDatum'), which will identifies the 'DsConfDatum'. Then, this utxo
will be used as a reference input for the validator so the validator knows
the minting policy hash.

So initially, given a utxo, we initialize the system as follows (left side
is transaction inputs, and rightside are the outputs).

```
utxo ---------------> utxo with
---------------> DsConfDatum
---------------> hash of minting policy of keys
---------------> [other hashes of minting policies can be included here as well]
---------------> utxo which represents the node
Node { nKey = "", nEdge = "\255...\255"}
where we recall that `""` is the lower bound of message digests, and
`"\255...\255"` is the upper bound of message digests
```

Some notation is needed here. When we say

```
utxo which represents the node
Node { nKey = "", nEdge = "\255...\255"}
this means that there exists a utxo (sitting at the validator script
'mkInsertValidator') with
```

- Datum as `DsDatum {dsNext = "\255...\255" }` (i.e., the `nEdge`
  field)
- A TokenName (with CurrencySymbol as 'mkDsKeyPolicy') `""` (i.e., the
  `nKey` field)

It's clear how this defn. generalizes to other nodes.

Then, to insert a string `str`, we follow the procedure above and build
transactions that look like this

```
utxo which represents: utxo which represents
Node Node
{ nKey = str' ---consumed----> { nKey = str'
, nEdge = str'' } ---------------> , nEdge = str }
--------------->
---------------> utxo which represents
utxo with [as a reference input] ---------------> Node
DsConfDatum { nKey = str
hash of minting policy of keys , nEdge = str'' }
[...]
```

Wow! It's hopefully just that easy!
