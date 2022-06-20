import sys
import mlabs_utils



if __name__ == '__main__':
    if (len(sys.argv) < 5):
        print('Not enough arguments')
        exit()
    (skey_path, vkey_path, spo_key, sidechain_skey) = sys.argv[1:5]
    mlabs_utils.register_build(
        skey_path = skey_path,
        vkey_path = vkey_path,
        spo_key = spo_key,
        sidechain_skey = sidechain_skey,
        chain_id = 123,
        genesis_hash=112233
    )
