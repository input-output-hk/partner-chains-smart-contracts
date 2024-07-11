#!/bin/bash
echo "Waiting 20 seconds for cardano testnet to start..."
sleep 20
echo "Running ogmios..."
exec /bin/ogmios "$@"