#!/bin/sh
set -e

this=$(realpath $0)

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

[ -f $TMP_DIR/register.sig ] || {
  echo "WARNING: a register transaction was not invoked for this session"
  echo "         but one may exist on chain."
}

# this assumes signatures are valid, otherwise we need to implement equivalents
# of ownEntries, isSignatureValid, etc..

# See: OffChain.CommitteeCandidateValidator.deregister

cardano-cli transaction build $TESTNET_MAGIC \
  --babbage-era \
  --tx-in $(get_utxo $(get_script_addr $EXPORTS_DIR/CommitteeCandidateValidator.plutus) "script input") \
  --tx-in $(get_utxo $(get_own_wallet_addr) input "as much lovelace as script input") \
  --tx-in-script-file $EXPORTS_DIR/CommitteeCandidateValidator.plutus \
  --tx-in-datum-file $EXPORTS_DIR/CommitteeCandidateValidator.datum \
  --tx-in-redeemer-file $EXPORTS_DIR/CommitteeCandidateValidator.redeemer \
  --tx-in-collateral $(get_utxo $(get_own_wallet_addr) collateral) \
  --tx-out $(get_own_wallet_addr)+1050000 \
  --change-address $(get_own_wallet_addr) \
  --protocol-params-file $TMP_DIR/protocolParams.json \
  --out-file $TMP_DIR/deregister.raw

[ "$SUBMIT" = "submit" ] && {
cardano-cli transaction sign $TESTNET_MAGIC \
  --tx-body-file $TMP_DIR/deregister.raw \
  --signing-key-file "$SKEY_PATH" \
  --out-file $TMP_DIR/deregister.sig

cardano-cli transaction submit $TESTNET_MAGIC \
  --tx-file $TMP_DIR/deregister.sig
}
