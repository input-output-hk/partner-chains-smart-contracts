#!/bin/sh
set -e

this=$(realpath $0)
SUBMIT=$1

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

[ -f $TMP_DIR/mint.sig ] || {
  echo "WARNING: a mint transaction was not invoked for this session"
  echo "         but one may exist on chain."
}

POLICY=$EXPORTS_DIR/FUELMintingPolicy
TOKEN=$(cardano-cli transaction policyid --script-file $POLICY.plutus).$(printf FUEL | xxd -p)

TX_IN_TOK=$(get_utxo $(get_own_wallet_addr) burn "1 $TOKEN")
TX_IN_ADA=$(get_utxo $(get_own_wallet_addr) "fees & collateral")

cardano-cli transaction build $TESTNET_MAGIC \
  --babbage-era \
  --tx-in $TX_IN_ADA \
  --tx-in $TX_IN_TOK \
  --tx-in-collateral $TX_IN_ADA \
  --tx-out "$(get_own_wallet_addr)+1050000" \
  --mint "-1 $TOKEN" \
  --mint-script-file $POLICY.plutus \
  --mint-redeemer-file $POLICY.burn.redeemer \
  --change-address $(get_own_wallet_addr) \
  --protocol-params-file $TMP_DIR/protocolParams.json \
  --out-file $TMP_DIR/burn.raw

[ "$SUBMIT" = "submit" ] && {
cardano-cli transaction sign $TESTNET_MAGIC \
  --tx-body-file $TMP_DIR/burn.raw \
  --signing-key-file "$SKEY_PATH" \
  --out-file $TMP_DIR/burn.sig

cardano-cli transaction submit $TESTNET_MAGIC \
  --tx-file $TMP_DIR/burn.sig
}
