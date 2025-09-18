# Partner Chains Smart Contracts

[![Build Status](https://github.com/input-output-hk/partner-chains-smart-contracts/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/input-output-hk/partner-chains-smart-contracts/actions/workflows/ci.yml)
[![Made with Haskell](https://img.shields.io/badge/Made%20with-Haskell-5e5086?logo=haskell)](https://www.haskell.org/)
[![Built with Nix](https://img.shields.io/badge/Built%20with-Nix-5277C3?logo=nixos)](https://nixos.org/)
[![Haddock Docs](https://img.shields.io/badge/docs-haddock-blue)](https://input-output-hk.github.io/partner-chains-smart-contracts/partner-chains-smart-contracts/index.html)

## :book: Overview

This repository contains the on-chain scripts that are part of the [Partner Chains framework](https://github.com/input-output-hk/partner-chains).
The Partner Chains Toolkit provides features for maintaining and securing Substrate based blockchains with the Cardano ecosystem.

## :books: Documentation

- **Haddock API Docs**: [partner-chains-smart-contracts](https://input-output-hk.github.io/partner-chains-smart-contracts/partner-chains-smart-contracts/index.html)
- **Changelog** [CHANGELOG.md](./CHANGELOG.md)

## :rocket: Getting Started

The project provides a [Nix](https://nixos.org/nix) environment that provides all necessary tools
and dependencies for building it (You can also choose to install dependencies manually but we won't
officially support or document this approach).

```bash
$ nix develop # enter the development shell
$ cd onchain
$ make
```
**Note**: The first invocation of `nix develop` may take some time since it fetches all required
dependencies.

## :scroll: License

This project is licensed under the **Apache 2.0 license**.
You are free to copy, modify, and distribute it under the terms of that license.

See the [LICENSE](./LICENSE) and [NOTICE](./NOTICE) files for details.
