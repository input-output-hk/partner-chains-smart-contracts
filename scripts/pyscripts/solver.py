import utils

""" WIP """

"""
This could probably use some backtracking solution, instead of mutating available utxos
Note: always prefer least number of utxos, so actually some of the code below is wrong..
it should be that we start from the biggest utxo and only resort to smaller ones when the
biggest isn't enough. This actually should simplify things(?) (but surely fragment the wallet so much)
"""

"""
solve : ([Token], address, magic) -> {
    TokenName : [utxo], ...
    "collateral" : utxo,
} | None
Token: {
    "name": TokenName,
    "amount": amount,
}
"""
def solve(tokens, address, magic=9):
    status, utxos = utils.get_utxos(address, magic)
    assert status == 'ok', utxos

    out = {}

    for token in tokens:
        balanced = want(token, utxos)
        for utxo in balanced: utxos.pop(utxo)
        if not balanced: return
        out[token] = balanced

    out['collateral'] = [out['lovelace'][-1]] \
        or want({"name" : "lovelace", "amount" : 0}, utxos)[0] # TODO: amount should be min lovelace
    # too conservative, can get collateral from the same utxo if it's big enough

    return out



def want(token, utxos): # (Token, [Map utxo {value: {TokenName : Nat}}]) -> [utxo]
    tokens = {}
    for utxo, data in utxos.items():
        name = token['name']
        value = data['value']
        if name in value: tokens[utxo] = value[name]

    return balance(token['amount'], tokens)

def balance(want, have): # (Nat, [Map utxo Nat]) -> [utxo]
  return []
