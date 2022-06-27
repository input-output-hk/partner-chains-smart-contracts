import argparse
import solver
import utils
from os.path import join, splitext

def doexport(args):
    if args.tx_in is None:
        status, addr = utils.get_address(args.addr_path, magic=79)
        assert status == 'ok', addr
        status, json = utils.get_utxos(addr, magic=79)
        assert status == 'ok', json
        args.tx_in = json.keys()[0]
    result = utils.export(
        args.tx_in,
        args.chain_id,
        args.genesis_hash, # '0x01e3ce523edd00cefe9c06631a55a8877e5ae354eb1abcbb8d67e7a2ed87e95c'
        args.spo_key,
        args.sidechain_skey,
    )
    print(result)

def dobuild(args):
    exports = lambda x: join(utils.get_project_root(), 'exports', x)
    config = {
        "secret_keyfile" : args.skey_path,
        "public_keyfile" : args.addr_path,
        "magic" : 79,
        "era" : "babbage",
        "with_submit" : args.submit,
    }
    custom =  {
        "register" : {
            "args": ([],"", "", "register"),
            "kwargs": {
                "inline_datum": exports('CommitteeCandidateValidator.datum'),
            }
        },
        "deregister" : {
            "args": ([],"", "", "deregister"),
            "kwargs": {
                "script": exports('CommitteeCandidateValidator.plutus'),
                "datum": exports('CommitteeCandidateValidator.datum'),
                "redeemer": exports('CommitteeCandidateValidator.redeemer'),
            }
        },
        "mint" : {
            "args": ([],"", "", "mint"),
            "kwargs": {
                "mint_val": utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL'),
                "mint_script": join(exports, 'FUELMintingPolicy.plutus'),
                "mint_redeemer": join(exports, 'FUELMintingPolicy.mint.redeemer'),
            }
        },
        "burn" : {
            "args": ([],"", "", "burn"),
            "kwargs": {
                "mint_val": utils.get_value(exports('FUELMintingPolicy.plutus'), 'FUEL'),
                "mint_script": join(exports, 'FUELMintingPolicy.plutus'),
                "mint_redeemer": join(exports, 'FUELMintingPolicy.burn.redeemer'),
            }
        },
    }
    for action in args.actions:
        options = custom[action]
        result = build(*options['args'], **options['kwargs'], **config)
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
                        default='',
                        help='Provide spo_key',
                        type=str
                        )
    export.add_argument('-SideK', '--sidechain-skey-file',
                        default='',
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
