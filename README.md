# Trustless Sidechain - Plutus contracts

## Introduction

This repository is a Plutus application (off-chain code + on-chain Plutus scripts) that is part of
the Sidechain Bridge Backend. The goal is to support the different interactions with Cardano as a
mainchain from a sidechain's perspective.

The repository targets the following users:

* the Sidechain node operator
* the end-user wishing to transfer assets between the mainchain and the sidechain

## Getting started

See our [user manual](./offchain/README.md) for the list of available commands that you can use to
interact with Cardano.

## Architecture and design

Read the [architecture documentation](./docs/Architecture.md) for an explaination on the components
involved in the mainchain component of a sidechain.

Read [the specification of the Plutus contracts](./docs/Specification.md) that are involved when
interacting with the Cardano mainchain.

For all proposals to significantly change the general architecture and design, see our
[SIPs](./docs/SIP-index.md).

## Other sidechain components

See our Sidechain bridge [integration guidelines](./docs/IntegrationGuideline.md) on how to
integrate this CLI with the various Sidechain bridge backend events.

See the [PoS Sidechain Technical
specification](https://docs.google.com/document/d/1UJs4ews1wnKIv4RMyPjFtJcyniyRHi7GmU2JPdUfbQk) and
[POC EVM Sidechain technical
specification](https://github.com/input-output-hk/sc-evm/blob/main/TECHNICAL_SPECIFICATION_v1.1.pdf)
for an in-depth explanation of all the components required to run a sidechain.

We also recommend reading the [Proof-of-State Sidechains white
paper](https://eprint.iacr.org/2018/1239.pdf) which spawned the idea of Sidechains.

## CONTRIBUTING

See [CONTRIBUTING](./CONTRIBUTING.md) for guidelines on contributing code and documentation.

See our [CHANGELOG](./CHANGELOG.md) for all changes over time to the tool.

## LICENSING

You are free to copy, modify, and distribute the trustless-sidechain project under the terms of the
Apache 2.0 license.

See the [LICENSE](./LICENSE) and [NOTICE](./NOTICE) files for details.
