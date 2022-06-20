import argparse
import mlabs_utils



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('-SKP', '--skey-file',
                        dest='skey_path',
                        default='/home/christos/IOHK/repos/mamba-world/SPOs/spo1/payment.skey',
                        help='Provide skey_path',
                        type=str
                        )
    parser.add_argument('-ADDR', '--payment-addr-file',
                        dest='addr_path',
                        default='/home/christos/IOHK/repos/mamba-world/SPOs/spo1/payment.addr',
                        help='Provide payment address path',
                        type=str
                        )
    parser.add_argument('-SPOK', '--spo-key-file',
                        dest='spo_key',
                        default='/home/christos/IOHK/repos/mamba-world/SPOs/spo1/cold.skey',
                        help='Provide spo_key',
                        type=str
                        )
    parser.add_argument('-SideK', '--sidechain-skey-file',
                        default='/home/christos/IOHK/repos/mamba-world/SPOs/mamba1/leader_private.key',
                        dest='sidechain_skey',
                        help='Provide sidechain_skey',
                        type=str
                        )

    args = parser.parse_args()

    mlabs_utils.register_build(
        skey_path = args.skey_path,
        addr_path = args.addr_path,
        spo_key_file = args.spo_key,
        sidechain_skey_file = args.sidechain_skey,
        chain_id = 79,
        genesis_hash='0x01e3ce523edd00cefe9c06631a55a8877e5ae354eb1abcbb8d67e7a2ed87e95c'
    )
