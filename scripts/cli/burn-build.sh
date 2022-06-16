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
ROOT_DIR=$(realpath $SCRIPTS_DIR/../..)
EXPORTS_DIR=$ROOT_DIR/exports
TMP_DIR=$SCRIPTS_DIR/tmp

POLICY=$EXPORTS_DIR/FUELMintingPolicy
FUELTN="4655454c"

ADDR=$(cat $TMP_DIR/ownWallet.addr)

# mint
$SCRIPTS_DIR/mint-build.sh


# await tx confirmed
query_utxos() {
  cardano-cli query utxo --address $ADDR --testnet-magic 9 \
    --out-file $TMP_DIR/tmp.json
}

get_utxos() {
  jq -r "keys[0]" $TMP_DIR/ownUTXOs.json
}

OLD_TX_IN=$(get_utxos)

while [ $OLD_TX_IN = get_utxos ]
do query_utxos
done


# burn
cardano-cli transaction build \
  --babbage-era \
  --tx-in $TX_IN \
  --tx-in-collateral $TX_IN \
  --tx-out $(cat $POLICY.addr)+1020000 \
  --mint "-1 $(cardano-cli transaction policyid --script-file $POLICY.plutus).$FUELTN" \
  --mint-script-file $POLICY.plutus \
  --mint-redeemer-file $POLICY.burn.redeemer \
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
