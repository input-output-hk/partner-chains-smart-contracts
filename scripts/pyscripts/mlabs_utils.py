import os
import subprocess
import json
from typing import List

## General python Operations

def run(cmd):
    return bytes.decode(subprocess.check_output(cmd.split(' '), shell=True), 'utf-8')

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
        tx_ins: List[],
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

    run(cmd)

## Sidechain Operations
def register_build(
    skey_path: str,
    vkey_path: str,
    spo_key,
    sidechain_skey,
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

    # we can think on more generic things here
    cmd = f'cardano-cli address build --payment-script-file {exports_dir}/CommitteeCandidateValidator.plutus --{network}'
    committee_candidate_validator_addr = run(cmd)
    write_file(f'{exports_dir}/CommitteeCandidateValidator.addr', committee_candidate_validator_addr)


    cmd = f'cardano-cli address build --payment-verification-key-file {vkey_path} --{network}'
    own_wallet = run(cmd) 
    write_file(f'{tmp_dir}/ownWallet.addr', own_wallet)

    addr = own_wallet

    cmd = f'cardano-cli query utxo --address {addr} --{network} --out-file {tmp_dir}/ownUtxos.json'
    run(cmd)
    tx_in = json.loads(read_file(f'{tmp_dir}/ownUtxos.json'))['keys'][0]

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
