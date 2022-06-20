import argparse
import mlabs_utils



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-SKP', '--skey-path',
                        required=True,
                        dest='skey_path',
                        help='Provide skey_path',
                        type=str
                        )
    parser.add_argument('-VKP', '--vkey-path',
                        required=True,
                        dest='vkey_path',
                        help='Provide vkey_path',
                        type=str
                        )
    parser.add_argument('-SPOK', '--spo-key',
                        required=True,
                        dest='spo_key',
                        help='Provide spo_key',
                        type=str
                        )
    parser.add_argument('-SideK', '--sidechain-skey',
                        required=True,
                        dest='sidechain_skey',
                        help='Provide sidechain_skey',
                        type=str
                        )

    args = parser.parse_args()

    mlabs_utils.register_build(
        skey_path = args.skey_path,
        vkey_path = args.vkey_path,
        spo_key = args.spo_key,
        sidechain_skey = args.sidechain_skey,
        chain_id = 123,
        genesis_hash=112233
    )
