#!/bin/sh

CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
    "caID": {
        "tag": "RegisterCommitteeCandidate",
        "contents": {
          "sidechainParams": {
            "chainId": "00",
            "genesisHash": ""
          },
          "spoPkh": {
            "getPubKeyHash": "5e05b8642699bcb7d0ceaf6f99e2edc05d947b0b7cfd78359d4ce528"
          },
          "sidechainPubKey": ""
        }
    }
  }' )
  # }' | jq -r .unContractInstanceId )


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 


