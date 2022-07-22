#!/usr/bin/env python3

import argparse
import solver
import utils

def doexport(args):
    if args.tx_in is None:
        status, addr = utils.get_address(args.vkey_path, magic=args.magic)
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
        "public_keyfile" : args.vkey_path,
        "magic" : args.magic,
        "era" : "babbage",
        "with_submit" : args.submit,
    }
    status, addr = utils.get_address(args.vkey_path, magic=args.magic)
    print(f'Own address: {addr}')
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
            "mint_val": (args.mint_amount, utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL')),
            "mint_script": exports('FUELMintingPolicy.plutus'),
            "mint_redeemer": exports('FUELMintingPolicy.mint.redeemer'),
        },
        "burn" : {
            "mint_val": (-args.burn_amount, utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL')),
            "mint_script": exports('FUELMintingPolicy.plutus'),
            "mint_redeemer": exports('FUELMintingPolicy.burn.redeemer'),
        },
    }
    for action in args.actions:
        options = custom[action]
        result = solver.build(action, **options, **config)
        print(result)

def add_common_arguments(parser):
    required = parser.add_argument_group('required arguments')
    required.add_argument('-sk', '--signing-key-file',
                          dest='skey_path',
                          help='Provide payment signing key path',
                          type=str,
                          required=True,
                          )

    parser.add_argument('-vk', '--payment-verification-key-file',
                        dest='vkey_path',
                        help='Provide payment verification key path. In case no key is given, it will be derived from the signing key to the same directory.',
                        type=str
                        )
    required.add_argument('-tm', '--testnet-magic',
                        dest='magic',
                        help='Provide testnet magic number',
                        type=int,
                        )
    required.add_argument('-mn', '--mainnet',
                        dest='mainnet',
                        help='Use the mainnet magic id (overrides testnet magic)',
                        action=argparse.BooleanOptionalAction,
                        )


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    
    subparser = parser.add_subparsers(dest='verb')

    export = subparser.add_parser('export')
    build = subparser.add_parser('build')

    add_common_arguments(export)
    add_common_arguments(build)

    build.add_argument('actions',
                       nargs='+',
                       help='Legal values: [register|deregister|mint|build].. order is dependent',
                       type=str
                       )

    build.add_argument('-s', '--submit',
                       action='store_true',
                       help='Also sign and submit transactions after building',
                       dest='submit'
                       )

    build.add_argument('-ba', '--burn-amount',
                       nargs='?',
                       dest='burn_amount',
                       help='amount to burn; natural number',
                       default=1,
                       type=int
                       )
    build.add_argument('-ma', '--mint-amount',
                       nargs='?',
                       dest='mint_amount',
                       help='amount to burn; natural number',
                       default=1,
                       type=int
                       )

    export.add_argument('-txin', '--tx-input',
                        dest='tx_in',
                        help='The input UTXO used to initiate the sidechain',
                        type=str
                        )
    export.add_argument('-ci', '--sidechain-id',
                        dest='chain_id',
                        type=int,
                        required=True
                        )
    export.add_argument('-gh', '--sidechain-genesis-hash',
                        dest='genesis_hash',
                        type=str,
                        required=True
                        )
    export.add_argument('-spk', '--spo-signing-key-file',
                        dest='spo_key',
                        help='Provide spo_key',
                        type=str,
                        required=True
                        )
    export.add_argument('-sck', '--sidechain-signing-key-file',
                        dest='sidechain_skey',
                        help='Provide sidechain_skey',
                        type=str,
                        required=True
                        )

    args = parser.parse_args()
    if args.vkey_path is None:
        status, vkey_path = utils.mk_vkey(args.skey_path)
        assert status == 'ok', "Couldn't derive verification key"
        args.vkey_path = vkey_path

    if args.mainnet:
        args.magic = 764824073

    match = {
        'export': doexport,
        'build': dobuild,
    }
    match.get(args.verb, lambda _: parser.print_usage())(args)

