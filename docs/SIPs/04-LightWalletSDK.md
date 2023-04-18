# Light Wallet SDK

In our future light wallet implementation, we should decide which endpoints we want to support in a browser, and which endpoints are only accessible via the CLI.

In my reasoning for the decisions, I have identified 3 actors:

- end users
- block producers
- bridge (automated calls)

In my opinion, only the end-user requires a browser based approach to conveniently interact with the chain, other actors will be comfortable with the CLI.

Based on this reasoning, these are the endpoint we should support:

- FUEL claim
- FUEL burn

As a reference, these are the endpoints that will be only supported through the CLI:

- initialization
- committee handover, Merkle root insertion and checkpoint insertion
- committee candidate registration/deregistration

## Implementation of the JavaScript SDK

The below code examples are just for reference, the resulting SDK might, but the general direction is something like this.

### Configuration

`SidechainParameters` are configured in JavaScript after script import

```jsx
import sidechainSDK from "sidechain-sdk"

const sidechainParams = {chainId: 123, genesisHash: "aabbcc", ...}
const sidechain = sidechainSDK.setup(sidechainParams)

// Or with custom wallet configuration
const sidechain = sidechainSDK.setup(sidechainParams, "lode", "preview")
```

### FUEL claim flow

1. Bridge generates a `CombinedMerkleProof`
2. Importing this data to a website
3. Sign and submit transaction with a light wallet

```jsx
const combinedProof = "00112233"
const txHash = await sidechain.claimFUEL(combinedProof)
```

### FUEL burn flow

1. Website requires user input: token amount and sidechain recipient address
2. Website constructs transaction to burn FUEL tokens
3. Sign and submit with a lightwallet

```jsx
const amount = 10
const recipient = "0xaabbcc"

const txHash = await sidechain.burnFUEL(amount, recipient)
```
