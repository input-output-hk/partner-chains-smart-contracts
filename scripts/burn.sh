#!/bin/sh

CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
    "caID": {
        "tag": "BurnFUELToken",
        "contents": {
          "sidechainParams": {
            "chainId": "00",
            "genesisHash": ""
          },
          "amount": -10,
          "recipient": ""
        }
    }
  }' | jq -r .unContractInstanceId )


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 


