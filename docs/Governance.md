Governance
==========

Certain operations on the blockchain require approval from the governance
authority:

  1. Minting new versioning tokens for the purposes of inserting new script
     versions.
  2. Burning versioning tokens for the purposes of invalidating existing script
     versions.
  3. Updating existing scripts to new versions.

Current implementation contains only a scaffold of the governance mechanism.  It
assumes existence of a single master payment public key, whose signature is
required on transactions that require governance approval, and that public key
is identical to the key of a stakeholder that initialized the sidechain in the
adoption phase.
