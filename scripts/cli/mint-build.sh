#!/bin/sh
set -e

this=$(realpath $0)
SUBMIT=$1

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

POLICY=$EXPORTS_DIR/FUELMintingPolicy
TOKEN=$(cardano-cli transaction policyid --script-file $POLICY.plutus).$(printf FUEL | xxd -p)

cardano-cli transaction build $TESTNET_MAGIC \
  --babbage-era \
  --tx-in $(get_utxo $(get_own_wallet_addr) input "1050000 lovelace") \
  --tx-in-collateral $(get_utxo $(get_own_wallet_addr) collateral) \
  --tx-out "$(get_own_wallet_addr)+1050000+1 $TOKEN" \
  --mint "1 $TOKEN" \
  --mint-script-file $POLICY.plutus \
  --mint-redeemer-file $POLICY.mint.redeemer \
  --change-address $(get_own_wallet_addr) \
  --protocol-params-file $TMP_DIR/protocolParams.json \
  --out-file $TMP_DIR/mint.raw

[ "$SUBMIT" = "submit" ] && {
cardano-cli transaction sign $TESTNET_MAGIC \
  --tx-body-file $TMP_DIR/mint.raw \
  --signing-key-file $SKEY_PATH \
  --out-file $TMP_DIR/mint.sig

cardano-cli transaction submit $TESTNET_MAGIC \
  --tx-file $TMP_DIR/mint.sig
}
