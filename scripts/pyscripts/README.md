
# How to run:

    python main.py -h
    python main.py {verb} -h

# Examples:

You need to run export first before you can call the register, mint, etc endpoints
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr export
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build register mint

# More examples:
## Build and submit register-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build -s register
## Only build deregister-transaction
    python main.py -SKP ~/testnet/mykey.skey -ADDR ~/testnet/mykey.addr build deregister
## Build and submit mint-transaction when the address is ~/testnet/mykey.vkey
    python main.py -SKP ~/testnet/mykey.skey build -s mint

# Scripts:

* main.py: entrypoint
* utils.py: cardano interfacing
* solver.py: constraint solver for getting input utxos etc..
