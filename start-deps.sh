#!/bin/bash

CARDANO_WORK_DIR=cardano-preview-workdir
CARDANO_CONFIG_DIR=cardano-preview-config

case $1 in
    cardano-node)
        cardano-node run \
        --topology      $CARDANO_CONFIG_DIR/topology.json \
        --config        $CARDANO_CONFIG_DIR/config.json \
        --database-path $CARDANO_WORK_DIR/db \
        --socket-path   $CARDANO_WORK_DIR/node.socket \
        --host-addr 0.0.0.0 \
        --port 3001
    ;;
    kupo)
        kupo \
        --node-socket $CARDANO_WORK_DIR/node.socket \
        --node-config $CARDANO_CONFIG_DIR/config.json \
        --host 127.0.0.1 \
        --port 1442 \
        --since origin \
        --prune-utxo \
        --defer-db-indexes \
        --match "*" \
        --workdir $CARDANO_WORK_DIR/kupo
    ;;
    ogmios)
        ogmios \
        --node-socket $CARDANO_WORK_DIR/node.socket \
        --node-config $CARDANO_CONFIG_DIR/config.json \
        --host 127.0.0.1 \
        --port 1337
    ;;
esac
