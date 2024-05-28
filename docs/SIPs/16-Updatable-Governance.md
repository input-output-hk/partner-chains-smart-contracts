Updatable Governance
====================

The purpose of this document is to describe a design for a sidechain security
mechanism, which we will refer to as the *governance*.  The purpose of
governance is to authorize certain actions that modify a sidechain, either an
already running one, or one that is only in the process of being initialized.

Requirements
------------

This document is being written in response to concrete requirements from the
client:

  * A default securing mechanism is a N-of-M multi-signature where the partner
    chain developer is responsible for defining the values of N, M and the
    wallets in question.

  * This should be configurable to allow arbitrary logic
    - The partner chain developer may have a DAO in place
    - They may be happy with a "proof of authority" solution with a manager NFT

  * This should be upgradable (in case the partner chain developer eventually
    gets a governance mechanism to replace the default, or makes an upgrade to a
    previous mechanism)

  * Upgrading this mechanism should require the prover to show they can meet at
    least the current security threshold.

Current implementation
----------------------

In the current implementation with have a placeholder governance mechanism that
requires a single signature by the authority specified in the sidechain's
configuration file.  Typically, this will be the same entity that burns the
genesis UTxO.

As of the writing of this SIP the governance approval is required for:

  * Managing the D-parameter and permissioned candidates list.

  * Managing the protocol version, i.e. inserting and removing scripts from the
    versioning system.  Note that initializing a script, i.e. inserting it for
    the first time, does not require governance approval because it uses init
    tokens (per SIP-06).

The existing mechanism can be thought of as "admin access", i.e. a single entity
makes all the administrative decisions.

Planned changes to current implementation
-----------------------------------------

To implement the requirement for updatable governance we must change the current
implementation of the versioning system.  Therefore, all versioning management
actions that required placeholder governance approval in the current
implemntation will now check for approval from the updatable governance.

Management of the D-parameter and the permissioned candidates list will remain
unchanged, because the scripts are already in use by the clients.

Proposed design
---------------

The securing mechanism will be implemented in a form of a minting policy.  That
minting policy will verify an arbitrary security logic, for example an N-of-M
multisig or an NFT presence, and if the security condition holds it will mint at
least one token to denote governance approval of the transaction.  Token name
can be arbitrary.

Modularity will be achieved by storing the governance minting policy in the
versioning system.  When checking for governance approval, we will need to know
which version of the governance should be checked.  We will add this version
information to the redeemer of every command that requires governance approval.
From the perspective of trustless-sidechain CLI it seems best to add a dedicated
flag to every command that requires governance approval.

Since governance minting policy will be stored in the versioning system,
updating the governance will require governance approval.  Initializing the
governance script, i.e. uploading the first governance script to the versioning
system, will be done using an init token (c.f. SIP-06).  This does not
require governance approval, which is crucial to avoid circular dependency.

The default N-of-M multi-signature
----------------------------------

The minting policy for N-of-M multi-signature will be parameterized by a data
type that stores public keys of governance members as well as the number of
signatories required to consider an action approved:

```haskell
data MultisigGovernanceParams = MultisigGovernanceParams {
  { governanceMemebers :: [PubKeyHash]
  , requiredSignatures :: Integer
  }
```

**NOTE:** if parameters are set such that `requiredSignatures > length
governanceMemebers`, this will make approving of any action impossible.  This
needs to be clearly documented.

The governance policy will then mint a single token if the transaction is signed
by at least the required number of signatories.

### User interface

Every transaction that requires the governance approval will obviously require a
sufficient number of signatures from the governance.  To this end, every command
that requires the governance approval will print a serialized transaction body
in a format accepted by cardano-cli.  It will be the user's responsibility to
distribute the transaction to governance members, have them sign the transaction
with their private keys, and then send back the (signature, public key) pairs.
Once a sufficient number of signatures is acquired, the entity that originally
created the transaction will be responsible for submitting it to the network,
along with the obtained signatures.

**NOTE:** At this moment it is unclear to us whether the exact signatories need
to be indicated up-front when building the transaction.
