import os
import subprocess
import json
from typing import List

## General python Operations

# CARDANO_CLI_PATH = '/nix/store/lay4bvzmlknbr1h6ph4bc1bhnww9gbdg-devshell-dir/bin/'
CARDANO_CLI_PATH = ''

def run(cmd):
    return bytes.decode(subprocess.check_output(cmd.split(' '), shell=True), 'utf-8')

def run_cli(command, key=''):
    with subprocess.Popen(CARDANO_CLI_PATH + command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, shell=True) as process:
        stdout, stderr = process.communicate()
        stdout = stdout.decode("utf-8")
        stderr = stderr.decode("utf-8")
        print(stdout)
        print(stderr)
        if not stderr == '':
            return (-1)
    if not key == '':
        try:
            result = json.loads(stdout)[key]
            return result
        except:
            print('Error: Request return not in JSON format or key ', key, ' doesn\'t exist')
            return(-1)
    return stdout

def write_file(path, content):
    with open(path, 'w') as file:
        file.write(content)
def read_file(path):
    res=''
    with open(path, 'r') as file:
        res = file.read()
    return res

## Cardano cli and other Tools

def build_transaction(
        tx_ins: List,
        tx_out,
        change_address,
        out_file,
        tx_out_datum_hash_file=None,
        network='testnet-magic 9'
  ):
    cmd =  f'cardano-cli transaction build '
    cmd += f'--babbage-era '
    for tx_in in tx_ins:
        cmd += f'--tx-in {tx_in} '
    cmd += f'--tx-out {tx_out} '
    if tx_out_datum_hash_file:
        cmd += f'--tx-out-datum-hash-file {tx_out_datum_hash_file} '
    cmd += f'--change-address {change_address} '
    cmd += f'--{network} '

    cmd += f'--out-file {out_file} '

    run_cli(cmd)

## Sidechain Operations
def register_build(
    skey_path: str,
    addr_path: str,
    spo_key_file,
    sidechain_skey_file,
    chain_id = 123,
    genesis_hash=112233,
    network='testnet-magic 9'
):
    ## Here some manipulation with the path, instead of working with the current directory
    ## the path manipulation is done from the file's path. I hope that's ok
    this_file_path = os.path.dirname(os.path.realpath(__file__))

    root_dir = os.path.realpath(this_file_path+'../..')
    scripts_dir = this_file_path
    exports_dir = f'{root_dir}/exports'
    tmp_dir = f'{scripts_dir}/tmp'
    if not os.path.exists(tmp_dir):
        os.mkdir(tmp_dir)
    if not os.path.exists(exports_dir):
        os.mkdir(exports_dir)
    if not os.path.exists(addr_path):
        print('ERROR: Payment address file not found')
    if not os.path.exists(spo_key_file):
        print('ERROR: SPO cold.skey file not found')
    if not os.path.exists(sidechain_skey_file):
        print('ERROR: Sidechain SKEY file not found')

    with open(addr_path, 'r') as file:
        addr = file.read().strip()

    with open(spo_key_file, 'r') as file:
        spo_key = json.load(file)['cborHex']

    with open(sidechain_skey_file, 'r') as file:
        sidechain_skey = file.read().strip()

    # we can think on more generic things here
    cmd = f'cardano-cli address build --payment-script-file {exports_dir}/CommitteeCandidateValidator.plutus --{network}'
    committee_candidate_validator_addr = run_cli(cmd)
    write_file(f'{exports_dir}/CommitteeCandidateValidator.addr', committee_candidate_validator_addr)


    # cmd = f'cardano-cli address build --payment-verification-key-file {vkey_path} --{network}'
    # print(cmd)

    # own_wallet = run_cli(cmd)
    # write_file(f'{tmp_dir}/ownWallet.addr', own_wallet)

    # addr = own_wallet

    cmd = f'cardano-cli query utxo --address {addr} --{network} --out-file {tmp_dir}/ownUtxos.json'

    run_cli(cmd)
    tx_in = json.loads(read_file(f'{tmp_dir}/ownUtxos.json')).keys()
    tx_in = list(tx_in)
    tx_in = tx_in[0]

    os.chdir(root_dir)
    cmd = f'cabal run trustless-sidechain-export -- {tx_in} {chain_id} {genesis_hash} {spo_key} {sidechain_skey}'
    run(cmd)
    os.chdir(scripts_dir)

    build_transaction(
        tx_ins=[tx_in],
        tx_out=committee_candidate_validator_addr+'+1020000',
        tx_out_datum_hash_file=f'{exports_dir}/CommitteeCandidateValidator.datum',
        change_address=addr,
        out_file = f'{tmp_dir}/tx.raw',
        network=network
  )
