# Trustless Sidechain CTL

## Notice

As of 2023, IOG will no longer be updating or maintaining this repo.

After three months of experimentation with the proof-of-concept EVM sidechain testnet, we have gathered valuable learnings from this experience and this innovative playground. New use cases and functionality were tested, feedback from the community was gathered, and we are now shifting our focus back to the strategic evolution of our sidechains approach.

All information included below is considered publicly available and is free to be leveraged by developers to fork it to build and experiment with their own EVM sidechain solution. Should you have any questions, please reach out to our team on our IOG Discord server.

## Description
This specification details the main chain contract of a trustless sidechain system. The work relies on the BLS ATMS signature scheme, so we decided to implement the contract in two phases:

- Phase 1: MVP using an append-only signature scheme
- Phase 1.5: script optimizations and security improvements
- Phase 2: using ATMS signature scheme.

## Next steps.

If you decide to investigate further, or add your own development, start with the [development instructions](DEVELOPMENT.md) and go from there.

## Hardware and OS Requirements
Trustless Sidechain CTL should run on any modern OS and hardware combination capable of running a Node installation:
- Linux
- macOS
- Windows 10+

## Prerequisites

In order to run Trustless Sidechain CTL you also need to set up the runtime dependencies:

- [ogmios](https://github.com/cardanosolutions/ogmios)
- [kupo](https://cardanosolutions.github.io/kupo)
