#!/usr/bin/env python3

import argparse
import solver
import utils
from os.path import join, splitext

def doexport(args):
    if args.tx_in is None:
        status, addr = utils.get_address(args.addr_path, magic=args.magic)
        assert status == 'ok', addr
        status, json = utils.get_utxos(addr, args.magic)
        assert status == 'ok', json
        args.tx_in = next(iter(json.keys()))
    result = utils.export(
        args.tx_in,
        args.chain_id,
        args.genesis_hash, # '0x01e3ce523edd00cefe9c06631a55a8877e5ae354eb1abcbb8d67e7a2ed87e95c'
        args.spo_key,
        args.sidechain_skey,
    )
    print(result)

def dobuild(args):
    exports = utils.exports
    config = {
        "secret_keyfile" : args.skey_path,
        "public_keyfile" : args.addr_path,
        "magic" : args.magic,
        "era" : "babbage",
        "with_submit" : args.submit,
    }
    status, addr = utils.get_address(args.addr_path, magic=args.magic)
    assert status == 'ok', addr
    config['own_addr'] = addr
    custom =  {
        "register" : {
            "out_inline_datum": exports('CommitteeCandidateValidator.datum'),
            "out_script": exports('CommitteeCandidateValidator.plutus'),
        },
        "deregister" : {
            "in_script": exports('CommitteeCandidateValidator.plutus'),
            "in_inline_datum": True,
            "in_redeemer": exports('CommitteeCandidateValidator.redeemer'),
        },
        "mint" : {
            "mint_val": (args.mintAmount, utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL')),
            "mint_script": exports('FUELMintingPolicy.plutus'),
            "mint_redeemer": exports('FUELMintingPolicy.mint.redeemer'),
        },
        "burn" : {
            "mint_val": (-args.burnAmount, utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL')),
            "mint_script": exports('FUELMintingPolicy.plutus'),
            "mint_redeemer": exports('FUELMintingPolicy.burn.redeemer'),
        },
    }
    for action in args.actions:
        options = custom[action]
        result = solver.build(action, **options, **config)
        print(result)


if __name__ == '__main__':
    parser = argparse.ArgumentParser()

    required = parser.add_argument_group('required arguments')
    required.add_argument('-SKP', '--skey-file',
                          dest='skey_path',
                          help='Provide skey_path',
                          type=str,
                          required=True,
                          )

    parser.add_argument('-ADDR', '--payment-addr-file',
                        dest='addr_path',
                        help='Provide payment address path',
                        type=str
                        )
    parser.add_argument('-MAGIC', '--testned-magic',
                        dest='magic',
                        help='Provide testnet magic number',
                        type=int,
                        default=79
                        )


    subparser = parser.add_subparsers(dest='verb')

    build = subparser.add_parser('build')
    build.add_argument('-s', '--submit',
                       action='store_true',
                       help='Also sign and submit transactions after building',
                       dest='submit'
                       )

    build.add_argument('actions',
                       nargs='+',
                       help='Legal values: [register|deregister|mint|build].. order is dependent',
                       type=str
                       )
    build.add_argument('-b', '--burnAmount',
                       nargs='?',
                       help='amount to burn; natural number',
                       default=1,
                       type=int
                       )
    build.add_argument('-m', '--mintAmount',
                       nargs='?',
                       help='amount to burn; natural number',
                       default=1,
                       type=int
                       )

    export = subparser.add_parser('export')
    export.add_argument('-TXIN', '--input-tx',
                        dest='tx_in',
                        help='The input UTXO used to initiate the sidechain',
                        type=str
                        )
    export.add_argument('-ChainID', '--sidechain-id',
                        dest='chain_id',
                        default=123,
                        type=int
                        )
    export.add_argument('-ChainH', '--sidechain-genesis-hash',
                        dest='genesis_hash',
                        default='112233',
                        type=str
                        )
    export.add_argument('-SPOK', '--spo-key-file',
                        dest='spo_key',
                        help='Provide spo_key',
                        type=str
                        )
    export.add_argument('-SideK', '--sidechain-skey-file',
                        dest='sidechain_skey',
                        help='Provide sidechain_skey',
                        type=str
                        )

    args = parser.parse_args()
    if args.addr_path is None:
        args.addr_path = splitext(args.skey_path)[0] + '.vkey'


    match = {
        'export': doexport,
        'build': dobuild,
    }
    match.get(args.verb, lambda _: parser.print_usage())(args)
