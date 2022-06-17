#!/bin/sh
set -e

this=$(realpath $0)

SCRIPTS_DIR=$(dirname $this)
TMP_DIR=$SCRIPTS_DIR/tmp

. $TMP_DIR/env

# deregister (TODO: get a datum for this)
