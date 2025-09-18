# Changelog

This changelog is based on [Keep A
Changelog](https://keepachangelog.com/en/1.1.0).


## 8.1.0 - 2025-09-16
### Changed
* Reserve validator no longer needs illiquidCirculationSupplyAddress UTXO ref input for TransferToIlliquidCirculationSupply and Handover redeemers
* ICS validator now requires illiquidCirculationSupplyAuthorityTokenPolicyId versioning oracle ref input
* ICS Validator does not allow to leak 'auth tokens' even if withdrawal policy token was minted
### Removed
* Removed checks for arbitrary token minting from Reserve validator
* Removed support for ADA as reserve token in Reserve validator and policy
* Removed check for other tokens from reserve minting policy
* Removed unit datum checks from ICS validator
* Removed unused scripts and helper functions

## 8.0.0 - 2025-07-31
### Added
* Trace enabled version of raw-scripts rust crate
### Changed
* Upgraded plutus-tx to 1.46.0.0 ([this introduces a fix for the plutus script double serialization bug](https://github.com/IntersectMBO/cardano-api/commit/86ac7198cfd69730fb0f28488a7e028feb268b70))
* Enabled plutus-tx optimization and trace removal in raw-scripts
### Removed
* t0 field from reserve immutable settings smart-contract

## 7.2.2 - 2025-06-02
### Fixed
* Fix cyclical dependency in versioning scripts

## 7.2.1 - 2025-05-29
### Changed
* add scriptid params to governed map validator and policy

## 7.2.0 - 2025-05-15
### Changed
* raw scripts are now wrapped with newtype RawScript

## 7.1.1 - 2025-04-22
### Changed
* GenericContainer changed to GovernedMap

## 7.1.0 - 2025-04-09
### Added
* Generic Containers smart contracts
### Removed
* Offchain directory removed

## 7.0.2 - 2025-01-10
### Added
* added `reserve-update-settings` command
* Example V function
* All script ids are now included in raw-scripts crate
### Changed
* wallet utxos with script refs are no longer used for balancing
* CTL deps updated
* `reserve-release-funds` now sets up validity interval from the current time to +infinity
### Removed
* Removed unused onchain types BlockProducerRegistration, BlockProducerRegistrationMsg, DParameterValidatorDatum, EcdsaSecp256k1PubKey, PermissionedCandidateKeys, PermissionedCandidatesPolicyRedeemer, PermissionedCandidatesValidatorDatum, PermissionedCandidatesValidatorRedeemer, PubKey, Signature, StakeOwnership
### Fixed
* Fixed error parser not parsing missing utxo error correctly

## 7.0.1 - 2024-11-14
### Removed
* Removed redundant --governance-authority from most commands (except init-governance) and the config

## v7.0.0

### Removed
* Removed `InitToken`
* Removed `init-tokens-mint` command
* Removed `init-token-status` command
* Removed `--chainId`, `--threshold-numerator` and `--threshold-denominator`
* Removed `SidechainParams` type

### Added
* Updateable governance system.
* `init-governance` command
* `update-governance` command

### Changed
* `DParameter`, `PermissionedCandidates` now use the updateable governance.
* Dropped `--version` parameter from all commands.
* Dropped `--old-version` and `--new-version` parameters from `update-version` command.
* Moved DParameter, Permissioned Candidates, CommitteeCandidateValidator, Reserve and IlliquidCirculationSupply to Generic Validators implementation.
* Serialization for `BlockProducerRegistrationMsg`
* Renamed `--genesis-committee-hash-utxo` to `--genesis-utxo`
