#!/bin/sh
set -e

this=$(realpath $0)
SUBMIT=$1

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

cardano-cli transaction build $TESTNET_MAGIC \
  --babbage-era \
  --tx-in $(get_utxo $(get_own_wallet_addr) input "1050000 lovelace") \
  --tx-out $(get_script_addr $EXPORTS_DIR/CommitteeCandidateValidator.plutus)+1050000 \
  --tx-out-datum-hash-file $EXPORTS_DIR/CommitteeCandidateValidator.datum \
  --change-address $(get_own_wallet_addr) \
  --out-file $TMP_DIR/register.raw

[ "$SUBMIT" = "submit" ] && {
cardano-cli transaction sign $TESTNET_MAGIC \
  --tx-body-file $TMP_DIR/register.raw \
  --signing-key-file "$SKEY_PATH" \
  --out-file $TMP_DIR/register.sig

cardano-cli transaction submit $TESTNET_MAGIC \
  --tx-file $TMP_DIR/register.sig
}
