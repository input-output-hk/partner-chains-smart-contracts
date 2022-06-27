import json
import os
import string
import subprocess
import tempfile
from functools import cache
from typing import List

## General python Operations

def option_then(o, f):
    return o and f(o) or ()

@cache
def getenv(extra_paths : List = []): # '/nix/store/lay4bvzmlknbr1h6ph4bc1bhnww9gbdg-devshell-dir/bin/'
    env = { **os.environ }
    extra_paths.append(env['PATH'])
    env['PATH'] = os.pathsep.join(extra_paths)
    return env

def run_cli(command, **kw):
    with subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        env=getenv(),
        **kw,
    ) as process:
        stdout, stderr = process.communicate()
        stdout = stdout.decode("utf-8")
        stderr = stderr.decode("utf-8")
        if stderr:
            return ('error', stderr)
    try:
        return ('ok', json.loads(stdout))
    except:
        return ('ok', stdout)

def write_file(path, content):
    with open(path, 'w') as file:
        file.write(content)
def read_file(path):
    with open(path, 'r') as file:
        res = file.read()
    return res

## Cardano cli and other Tools

@cache
def get_address(file, magic=9, type='verification-key'):
    cmd = [
        'cardano-cli', 'address', 'build',
        f'--testnet-magic={magic}',
        f'--payment-{type}-file={file}',
    ]
    return run_cli(cmd)

@cache
def get_params(magic=9):
    cmd = [
        'cardano-cli', 'query', 'protocol-parameters',
        f'--testnet-magic={magic}',
    ]
    return run_cli(cmd)

def get_utxos(addr, magic=9):
    with tempfile.NamedTemporaryFile(prefix='trustless-sidechain-', suffix='.json') as fd:
        cmd = [
            'cardano-cli', 'query', 'utxo',
            f'--testnet-magic={magic}',
            f'--address={addr}',
            f'--out-file={fd.name}'
        ]
        status, out = run_cli(cmd)
        if status == 'ok':
            return status, json.load(fd)
        else:
            return status, out

@cache
def get_project_root():
    path = os.path.realpath(
        os.path.join(__file__, os.path.pardir, os.path.pardir, os.path.pardir)
    )
    assert os.path.exists(os.path.join(path, 'cabal.project')), "got wrong root path for the script" + path
    return path

@cache
def get_value(script, name):
    if name == 'lovelace': return
    cmd = [
        'cardano-cli', 'transaction', 'policyid',
        f'--script-file={script}',
    ]
    status, out = run_cli(cmd)
    if status == 'error': return
    name = ''.join([hex(ord(c))[2:] for c in name])
    return f'{out}.{name}'


def export(tx_in, chain_id, genesis_hash, spo_key, sidechain_key):
    cmd = [
        'cabal', 'run', 'trustless-sidechain-export', '--',
        f'{tx_in}', f'{chain_id}', f'{genesis_hash}',
        f'{spo_key}', f'{sidechain_key}',
    ]
    return run_cli(cmd, cwd=get_project_root())

def build(
        tx_ins: List, # hash#index
        tx_out,       # hash#index+amount
        tx_coll,      # hash#index
        out_file,     # filepath
        era='babbage',
        magic=9,
        with_submit=False,
        **kw # script, datum, redeemer, inline_datum, mint_val, mint_script, mint_redeemer
  ):
    own_addr = get_address(kw['public_keyfile'], magic)
    params = get_params(magic)
    cmd = [
        'cardano-cli', 'transaction', 'build',
        f'--{era}-era',
        f'--testnet-magic={magic}',

        option_then(kw.get('script'), lambda x: f'--tx-in-script-file={x}'),
        option_then(kw.get('datum'), lambda x: f'--tx-in-datum-file={x}'),
        option_then(kw.get('redeemer'), lambda x: f'--tx-in-redeemer-file={x}'),
    ]

    for tx_in in tx_ins:
        cmd.append(f'--tx-in={tx_in}')

    cmd += [
        f'--tx-in-collateral={tx_coll}'
        f'--tx-out={tx_out}',

        option_then(kw.get('inline_datum'), lambda x: f'--tx-out-inline-datum-file={x}'),

        option_then(kw.get('mint_val'), lambda x: f'--mint={x}'),
        option_then(kw.get('mint_script'), lambda x: f'--mint-script-file={x}'),
        option_then(kw.get('mint_redeemer'), lambda x: f'--mint-redeemer-file={x}'),

        f'--change-address={own_addr}',
        f'--protocol-params-file={params}',
        f'--out-file={out_file}.raw',
    ]

    cmd = [arg for arg in cmd if arg != None] # get rid of null-options

    status, out = run_cli(cmd)

    if with_submit:
        status, out = status == 'ok' and sign(out_file, magic, **kw) or status, out
        status, out = status == 'ok' and submit(out_file, magic) or status, out

    return status, out

def sign(out_file, magic=9, secret_keyfile=None):
    cmd = [
        'cardano-cli', 'transaction', 'sign'
        f'--testnet-magic={magic}',
        f'--tx-body-file={out_file}.raw',
        f'--signing-key-file={secret_keyfile}'
        f'--out-file={out_file}.sig'
    ]
    return run_cli(cmd)

def submit(out_file, magic=9):
    cmd = [
        'cardano-cli', 'transaction', 'submit'
        f'--testnet-magic={magic}',
        f'--tx-file={out_file}.sig',
    ]
    return run_cli(cmd)
