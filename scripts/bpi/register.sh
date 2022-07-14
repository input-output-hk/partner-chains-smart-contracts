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
          "spoPubKey": {
            "getPubKey": "7ad00d01260b71e0b353dc041ded1ddf22c5dff225a90eb3dc0df5aa27e52aa2"
          },
          "sidechainPubKey": "",
          "spoSig": {
            "getSignature": "91a23645489b7d9c1004be8ef2c7570874906277efacb5881fcdf7995162cb896ebe4397eed7befcd4050671511dd3c1ed8f9b3b4c0c09ca0a1036ec6ff0bf00"
          },
          "sidechainSig": {
            "getSignature": ""
          },
          "inputUtxo": {
            "txOutRefId": {
              "getTxId": "d6c4e15ca74e5d45dd0aecfdcca9fb6eb85d7c46862ed5890e851be122a9d155"
            },
            "txOutRefIdx": 0
          }
        }
    }
  }' | jq -r .unContractInstanceId )


echo "{ \"tag\": \"Subscribe\", \"contents\": { \"Left\": { \"unContractInstanceId\":\"$CONTRACT_INST_ID\" } } }" | websocat -n ws://localhost:9080/ws 


