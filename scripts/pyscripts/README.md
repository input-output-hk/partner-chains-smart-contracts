# How to run:

```
python main.py -h
python main.py {verb} -h
```

# Examples:

You need to run export first before you can call the register, mint, etc endpoints
This step will compile the Plutus scripts with the given sidechain parameters, and also
generates datums and redeemers (json files) to be used by the transaction build command.

```
python main.py export \
--signing-key-file ~/testnet/mykey.skey \
--payment-verification-key-file ~/testnet/mykey.vkey \
--testnet-magic 9 \
--spo-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
--sidechain-signing-key fa832cc7ad4a0990f36db287df51c62a64c12287e161c07fbc8a4bde0b587c0a \
--sidechain-id 123 \
--sidechain-genesis-hash 112233 \
--genesis-tx-in 004bbb7e94b3ed5cd04d22fe4bf8f215a64c917bce12c68cbdc117d70efd4b98#0 \
--register-tx-in 004bbb7e94b3ed5cd04d22fe4bf8f215a64c917bce12c68cbdc117d70efd4b98#0
```

After that, you can build and optionally submit one or more transactions using the
following command:

```
python main.py build register mint \
--signing-key-file ~/testnet/mykey.skey \
--payment-verification-key-file ~/testnet/mykey.vkey \
--testnet-magic 9 \
--submit
```

# More examples:

## Build and submit register-transaction

```
python main.py build register \
--signing-key-file ~/testnet/mykey.skey \
--testnet-magic 9 \
--submit
```

## Only build deregister-transaction

```
python main.py build deregister \
--signing-key-file ~/testnet/mykey.skey \
--testnet-magic 9
```

## Build and submit 1 token mint-transaction

```
python main.py build mint \
--signing-key-file ~/testnet/mykey.skey \
--testnet-magic 9 \
--mint-amount 1 \
--submit
```

## Build 8 token burn-transaction

```
python main.py build burn \
--signing-key-file ~/testnet/mykey.skey \
--testnet-magic 9 \
--burn-amount 8
```

# Scripts:

- main.py: entrypoint
- utils.py: cardano interfacing
- solver.py: constraint solver for getting input utxos etc..
