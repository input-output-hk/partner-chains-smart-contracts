#!/bin/sh
set -e

this=$(realpath $0)
SUBMIT=$1

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

POLICY=$EXPORTS_DIR/FUELMintingPolicy
TOKEN=$(cardano-cli transaction policyid --script-file $POLICY.plutus).$(printf FUEL | xxd -p)

# God I wish we had constraint lookups
for o in $(get_utxos $(get_own_wallet_addr)) # also writes json to $TMP_DIR/pipe
do
  [ $TX_IN_ADA ] && [ $TX_IN_TOK ] && break
  if test "$TOKEN" = "$(
    jq -r \
    " .[\"$o\"].value | del(.lovelace)
                      | [..|keys?]
                      | flatten
                      | join(\".\")
    " $TMP_DIR/pipe
  )"
  then TX_IN_TOK=${TX_IN_TOK:-$o}
  else TX_IN_ADA=${TX_IN_ADA:-$o}
  fi
done

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
