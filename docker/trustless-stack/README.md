# Trustless Stack

This stack is designed to run all trustless-sidechain dependencies. The stack includes:

- 3 x cardano-node (local cardano testnet with, magic 42)
- 1 x ogmios
- 1 x kupo

## Setup and teardown

`wget -P bin https://www.busybox.net/downloads/binaries/1.35.0-x86_64-linux-musl/busybox`  # run only once

`docker-compose up -d`

`docker-compose down --volumes`

## Usage

### Manage containers
`nix-shell -p lazydocker --command lazydocker`

### Query cardano tip
`docker exec cardano-node-1 bash -c "cardano-cli query tip --testnet-magic 42"`

### Run any trustless-sidechain-cli command
Run the cli using any of the [available options](../../offchain/README.md#3-running-the-cli):
```bash
./sidechain-cli addresses \
--payment-signing-key-file ~/work/addresses/funded_address.skey \
--genesis-committee-hash-utxo 781cb948a37c7c38b43872af9b1e22135a94826eafd3740260a6db0a303885d8#0 \
--sidechain-id 0 \
--governance-authority e8c300330fe315531ca89d4a2e7d0c80211bc70b473b1ed4979dff2b \
--version 1 \
--threshold-numerator 2 \
--threshold-denominator 3 \
--ogmios-host localhost \
--ogmios-port 1337 \
--kupo-host localhost \
--kupo-port 1442 \
--atms-kind plain-ecdsa-secp256k1 | jq
```

## Config

### genesis.json
> path: docker/trustless-stack/config/local/genesis/shelley/genesis.json
```json
{
    "epochLength": 120,  # epoch length in slots
    "slotLength": 1,  # slot length in seconds, epoch = 120 * 1 = 2 minutes
}
```

### Docker ports
> path: docker/trustless-stack/.env

Change default ports of kupo ang ogmios, as well as containers' resources limits.

## Credits

Based on @skylar-simoncelli's [Partnerchains Substrate Node Stack](https://github.com/input-output-hk/sidechains-infra-priv/tree/master/src/docker/sidechains-subsrate-node-docker-stack) 