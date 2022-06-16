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

POLICY=$EXPORTS_DIR/FUELMintingPolicy
FUELTN="4655454c"
: <<HOW
ghci> :m +Cardano.Api
ghci> serialiseToRawBytesHex @AssetName "FUEL"
HOW

cardano-cli address build --payment-verification-key-file "$VKEY_PATH" --testnet-magic 9 > $TMP_DIR/ownWallet.addr

ADDR=$(cat $TMP_DIR/ownWallet.addr)

cardano-cli query utxo --address $ADDR --testnet-magic 9 --out-file $TMP_DIR/ownUtxos.json
TX_IN=$(jq -r "keys[0]" $TMP_DIR/ownUtxos.json )

cd $ROOT_DIR
cabal run trustless-sidechain-export -- $TX_IN $CHAIN_ID $GENESIS_HASH $SPO_SKEY $SIDECHAIN_SKEY
cd $SCRIPTS_DIR

cardano-cli address build --payment-script-file $POLICY.plutus --testnet-magic 9 > $POLICY.addr

cardano-cli transaction build \
  --babbage-era \
  --tx-in $TX_IN \
  --tx-in-collateral $TX_IN \
  --tx-out $(cat $POLICY.addr)+1020000 \
  --mint "1 $(cardano-cli transaction policyid --script-file $POLICY.plutus).$FUELTN" \
  --mint-script-file $POLICY.plutus \
  --mint-redeemer-file $POLICY.mint.redeemer \
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
