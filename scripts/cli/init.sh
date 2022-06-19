#!/bin/sh
set -ex

[ $# -ge 1 ] || {
	echo "usage: $0 <SKEY_PATH> [VKEY_PATH [SPO_SKEY] [SIDECHAIN_SKEY]]"
	false
}
this=$(realpath $0)

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp
ROOT_DIR=$(realpath $SCRIPTS_DIR/../..)

mkdir -p $TMP_DIR
> $TMP_DIR/pipe

cat <<ENV > $TMP_DIR/env
SKEY_PATH=$1
VKEY_PATH=${2:-${1%*.skey}.vkey}
SPO_SKEY=$3
SIDECHAIN_SKEY=$4

CHAIN_ID=123
GENESIS_HASH=112233

TESTNET_MAGIC="--testnet-magic 9"

ROOT_DIR=$ROOT_DIR
EXPORTS_DIR=$ROOT_DIR/exports

get_own_wallet_addr() {
  cat $TMP_DIR/ownWallet.addr
}

get_script_addr() {
  cardano-cli address build --payment-script-file \$1 \$TESTNET_MAGIC
}

# WARNING: bashisms(select)
get_utxo() {
  echo "need a utxo\${2:+ for \$2}\${3:+ with at least \$3}" >&2
  cardano-cli query utxo --address \$1 \$TESTNET_MAGIC >&2
  cardano-cli query utxo --address \$1 \$TESTNET_MAGIC --out-file $TMP_DIR/pipe
  select hash in \$(jq -r "keys[]" $TMP_DIR/pipe)
  do
    echo \$hash
    break
  done
}
ENV

. $TMP_DIR/env

cardano-cli address build --payment-verification-key-file "$VKEY_PATH" $TESTNET_MAGIC --out-file $TMP_DIR/ownWallet.addr
cardano-cli query protocol-parameters $TESTNET_MAGIC --out-file $TMP_DIR/protocolParams.json

TX_IN=$(get_utxo $(get_own_wallet_addr) exports)

cd $ROOT_DIR
  cabal run trustless-sidechain-export -- $TX_IN $CHAIN_ID $GENESIS_HASH $SPO_SKEY $SIDECHAIN_SKEY
cd $SCRIPTS_DIR
