#!/usr/bin/env bash

TEMP_DIR=$(mktemp -d)

export PURS_RAW_SCRIPTS_OUT=$TEMP_DIR/RawScripts.purs
export RS_RAW_SCRIPTS_OUT=$TEMP_DIR/lib.rs

EXPECTED_PURS=src/TrustlessSidechain/RawScripts.purs
EXPECTED_RS=../raw-scripts/src/lib.rs

echo ""
echo "Regenerating scripts"
echo ""
make -B update-scripts


echo ""
echo "Comparing PureScript output"
echo ""
if ! diff "$PURS_RAW_SCRIPTS_OUT" "$EXPECTED_PURS" ; then
    echo "Error: PureScript RawScript output differs!"
    exit 1
fi

echo ""
echo "Comparing Rust output"
echo ""
if ! diff "$RS_RAW_SCRIPTS_OUT" "$EXPECTED_RS" ; then
    echo "Error: Rust RawScript output differs!"
    exit 1
fi

