import utils
import math
import functools

"""
This could probably use some backtracking solution, instead of mutating available utxos.
Note: always prefer least number of utxos. The more of them you consume, the more likely your fees go up.
      it should be that we start from the biggest utxo and only resort to smaller ones when the biggest
      isn't enough. This actually should simplify things(?) (but surely fragment the wallet so much).
"""

"""
fee balancing, errors, last line: "Minimum required UTxO: Lovelace %d"
happens twice, but it's safe to do it with a fuel (10 times maybe) and break on ok
"""

def build(action, **kwargs):
    _, utxos = utils.get_utxos(kwargs['own_addr'])
    script = kwargs.get('script')
    mint = kwargs.get('mint_val')

    tx_ins = []
    to_mint = None

    if script:
        status, addr = utils.get_address(script, type='script', magic=kwargs['magic'])
        assert status == 'ok', addr
        _, tx = utils.get_utxos(addr, kwargs['magic'])
        tx_ins.append(next(iter(tx.keys())))

    if mint:
        amount, name = mint
        if mint[0] < 0:
            tx_ins += want({"name": name, "amount": amount}, utxos)
        else:
            to_mint = f'{amount} {name}'

    for utxo in tx_ins:
        if utxo in utxos: del utxos[utxo]

    amount = 0
    for _ in range(10):
        tx_in = want({"name": "lovelace", "amount": amount}, utxos)
        tx_out = f"{kwargs['own_addr']}+{amount}" + (f'+{to_mint}' if to_mint else '')
        tx_coll = tx_in[0]

        status, out = utils.build(tx_ins + tx_in, tx_out, tx_coll, action, **kwargs)
        if status == 'ok': return status, out

        _, _, lovelace = out.strip().rpartition(' ')
        try:
            amount = int(lovelace)
        except ValueError:
            amount = 0
            print(out)

    return status, out


def want(token, utxos): # (Token, [Map utxo {value: {TokenName : Nat}}]) -> [utxo]
    tokens = {}
    name = token['name']
    for utxo, data in utxos.items():
        value = data['value']
        if name in value: tokens[utxo] = int(value[name])

    return balance(token['amount'], tokens)

def balance(want, have): # (Nat, Map utxo Nat) -> [utxo]
    utxos = sorted(have.items(), reverse=True, key=lambda kv: kv[1])
    valid = filter(lambda kv: kv[1] >= want, utxos)
    try:
        utxo, _ = next(valid)
        return [utxo]
    except StopIteration:
        acc = []
        scan = (acc := acc + [utxo] for utxo in utxos)
        for summation in scan:
            valid, amount = functools.reduce(add_utxos, summation, initial=([], 0))
            if amount >= want: return valid
        return []

def add_utxos(acc, pair):
    utxos, amount = acc
    utxo, value = pair
    return utxos + [utxo], amount + value