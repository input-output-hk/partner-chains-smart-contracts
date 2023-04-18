# Permissioned Committee Candidate Registration

This design summarizes the modifications required for a permissioned committee candidate registration system.

## Requirements:

- modular system (user can decide whether she wants permissioned candidates or not)
- only a predefined subset of SPOs are able to register as candidates
- the permission can be transferred to other SPOs
- SPOs who have lost (transferred) their permission must be deregistered

## Solution

At sidechain initialization, the initializer *may* optionally mint a predefined amount of `CandidatePermissionToken`s,
and distribute them amongst the SPOs. Sidechain infrastructure must track sidechain initialization transactions, and
check for the minting of these permission tokens. These can be traded as regular native tokens. If no token is minted,
we consider the sidechain permissionless, any SPO can register.

These tokens require an arbitrary UTxO to be consumed (making the currency policy unique). This UTxO can be the same as
the genesisUtxo, but it’s not a requirement. The currency policy is stored and handled by the Bridge.

In registration, the token must be included and verified by the Bridge, if tokens were minted. Registration UTxOs without
a permission token where one is expected are considered invalid.

At deregistration, the token can be unlocked from the `CommitteeCandidateValidator`, and reused or traded at a later
time. Offchain infrastructure must continually validate that the tokens remain locked, deregistering candidates when
they remove their tokens.

A protocol update can later be used to change from permissoned to permissionless or vice versa, or even allowing minting
a new set of permission tokens. Indeed, we may recall that committee candidate registration / deregistration does not
interact with any of the other onchain systems and is used exclusively by the Bridge. Hence, this permits the Bridge to
arbitrarily change whether it accepts permissioned or permissionless registrations. Moreover, one may mint a new set of
permission tokens, and of course the Bridge may decide that these new tokens must be used for permissioned registrations.

This pattern can be used for *optional extensions* to the onchain protocol.
