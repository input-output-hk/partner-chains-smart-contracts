
# How to run:

    python main.py -h
    python main.py {verb} -h

# Examples:
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s register deregister
## Build and submit register-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s register
## Build and submit deregister-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s deregister
## Build and submit mint-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s mint --mintAmount 1
## Build and submit burn-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s burn --burnAmount 1
## Export
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr export

# Scripts:

* main.py: entrypoint
* utils.py: cardano interfacing
* solver.py: WIP constraint solver for getting input utxos etc..
