#!/bin/sh

CONTRACT_INST_ID=$(curl --location --request POST 'localhost:9080/api/contract/activate' \
  --header 'Content-Type: application/json' \
  --data-raw '{
    "caID": {
        "tag": "DeregisterCommitteeCandidate",
        "contents": {
          "sidechainParams": {
            "chainId": "00",
            "genesisHash": ""
          },
          "spoPubKey": {
            "getPubKey": "7ad00d01260b71e0b353dc041ded1ddf22c5dff225a90eb3dc0df5aa27e52aa2"
          }
        }
    }
  }' | jq -r .unContractInstanceId )

echo $CONTRACT_INST_ID


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 

