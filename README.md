# Trustless Sidechain CTL

## 1. Notice

As of [insert date], IOG will no longer be updating or maintaining this repo.

After three months of experimentation with the proof-of-concept EVM sidechain testnet, we have gathered valuable learnings from this experience and this innovative playground. New use cases and functionality were tested, feedback from the community was gathered, and we are now shifting our focus back to the strategic evolution of our sidechains approach.

All information included below is considered publicly available and is free to be leveraged by developers to fork it to build and experiment with their own EVM sidechain solution. Should you have any questions, please reach out to our team on our IOG Discord server.

## 2. Description
This specification details the main chain contract of a trustless sidechain system. The work relies on the BLS ATMS signature scheme, so we decided to implement the contract in two phases:

- Phase 1: MVP using an append-only signature scheme
- Phase 1.5: script optimizations and security improvements
- Phase 2: using ATMS signature scheme.

## 3. Next steps.

If you decide to investigate further, or add your own development, start with the [development instructions](DEVELOPMENT.md) and go from there.
