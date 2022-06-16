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

mkdir -p $TMP_DIR

cardano-cli address build --payment-script-file $EXPORTS_DIR/CommitteeCandidateValidator.plutus --testnet-magic 9 > $EXPORTS_DIR/CommitteeCandidateValidator.addr
cardano-cli address build --payment-verification-key-file "$VKEY_PATH" --testnet-magic 9 > $TMP_DIR/ownWallet.addr

ADDR=$(cat $TMP_DIR/ownWallet.addr)

cardano-cli query utxo --address $ADDR --testnet-magic 9 --out-file $TMP_DIR/ownUtxos.json
TX_IN=$(cat $TMP_DIR/ownUtxos.json | jq -r "keys | .[0]")

cd $ROOT_DIR
cabal run trustless-sidechain-export -- $TX_IN $CHAIN_ID $GENESIS_HASH $SPO_SKEY $SIDECHAIN_SKEY
cd $SCRIPTS_DIR


cardano-cli transaction build \
  --babbage-era \
  --tx-in $TX_IN \
  --tx-out $(cat $EXPORTS_DIR/CommitteeCandidateValidator.addr)+1020000 \
  --tx-out-datum-hash-file $EXPORTS_DIR/CommitteeCandidateValidator.datum \
  --change-address $ADDR \
  --testnet-magic 9 \
  --out-file $TMP_DIR/tx.raw

cardano-cli transaction sign \
  --tx-body-file $TMP_DIR/tx.raw \
  --signing-key-file "$SKEY_PATH" \
  --testnet-magic 9 \
  --out-file $TMP_DIR/tx.sig

cardano-cli transaction submit \
  --testnet-magic 9 \
  --tx-file $TMP_DIR/tx.sig
