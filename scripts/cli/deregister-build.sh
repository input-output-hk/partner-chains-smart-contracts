#!/bin/sh
set -e

me=$(realpath $0)

SKEY_PATH=$1
VKEY_PATH=$2

CHAIN_ID=123
GENESIS_HASH=112233
SPO_SKEY=$3
SIDECHAIN_SKEY=$4

SCRIPTS_DIR=$(dirname $me)
ROOT_DIR=$SCRIPTS_DIR/../..
EXPORTS_DIR=$ROOT_DIR/exports
TMP_DIR=$SCRIPTS_DIR/tmp

# register
$SCRIPTS_DIR/register-build.sh "$@"


ADDR=$(cat $TMP_DIR/ownWallet.addr)

query_utxos() {
  cardano-cli query utxo --address $ADDR --testnet-magic 9 \
    --out-file $TMP_DIR/tmp.json
}

get_utxos() {
  jq -r "keys[0]" $TMP_DIR/ownUTXOs.json
}

OLD_TX_IN=$(get_utxos)

# await tx confirmed
while [ $OLD_TX_IN = get_utxos ]
do query_utxos
done

# deregister (TODO: get a datum for this)
