# CNA Delegation Analysys

## 1. Introduction
CNA Delegation mechanism provides a way for partner chain block producers, and the whole partner chain consensus protocol to figure out the stake distribution.

## 2. Implementation
Delegation utxos are managed by the delegator's stake key, similar to how regular Ada delegation works on Cardano. However the built-in delegation mechanism doesn't allow for delegating to third-party block producers, and has no way of specifying partner chain reward address.

To solve this problem we've created MintingPolicy-Validator script pair. Minting Policy defines rules which the delegator has to follow in order to delegate. Validator defines rules which the delegator has to follow to cancell their's delegation.

Delegating means minting MinotaurStake Token, and sending it to the MinotaurStake Validator Address with the datum attached. Datum stores information about the partner-chain stake pool, partner-chain reward address and
delegator's stake pub key hash.

To get the stake distribution you have to:
1. Find all delegation utxos
2. Get the stake key hashes of all valid delegations
3. Find all utxos sitting at wallet addresses with the stake key part related to the stake key hash of the delegator
4. Count the amount of CNA tokens sitting at these addresses

## 3. Consequences Analysis
This design has some major benefits, and some drawbacks.

### 3.1 Benefits
- The whole system is not centralized. There is no single point of congestion. That means that many delegations can be made or cancelled in a single block.
- There is no way to create a fake delegation.
- The system currently works without specifying the CNA that will be staked. The specific CNA can be chosen at querying stake distribution stage, by whatever party want's to know the stake distributin. We assume that this party will know what the CNA is exactly. Right now there is no way of delegating different CNAs to different partner chains / stake pools. However we could easily parametrise the OnChain scripts with the CNA, and then we would get separate delegation mechanism instances for each CNA. This would also require the user to provide the specific CNA when delegating or cancelling delegation.

### 3.2 Drawbacks
- There is no way of preventing user from delegating multiple times to the same stake pool, or even to different stake pools using the same stake key hash. This problem can be solved at the stage of querying the stake distribution. We can assume, that only the latest delegation for given stake key hash is considered to be active. This works, but can be confusing for people who don't know this context.
- Current approach doesn't allow for updating the validation criteria. Because the whole mechanism doesn't have a notion of central authority, there is no way to easily create update mechanism. Updates normaly have to be approved by some governance authority, and right now we don't have clear idea what this authority would be exactly.
- We've created a [POC postgreSQL query](./CNADelegation.md) for getting the stake distribution for given stake pool. It work's but it's probably not very optimal. A single query takes a couple of seconds, and in the real scenario we would have to query stake distribution for all stake pools. It might not be a big problem, because it would be executed probably at most once a partner chain epoch, but it doesn't mean that there is no room for improvement.

### 3.3 Opportunities
- We could think about introducing a way to update the validation criteria in a safe way.
- We could try to figure out a way of preventing user for making multiple delegations. This probably would introduce some extra costs in terms of transaction fees, but also might make the querying stake distribution stage much simpler.
- We could try to create better SQL query for getting the stake distribution, or even splitting it into more subqueries that would be executed in a sequence by some other tool. This might improve the performance.

## 4. Impact Assessment
Assess the impact of the current approach on various aspects:

### 4.1 System Performance
Delegating and cancelling delegation are as fast, as subbmitting a single transaction is. Querying the stake distribution however right now is not very performant.

### 4.2 User Experience
For the delegator, the experience is good. The mechanism work's similar to other `trustless-sidechain` commands. Interface is simple on purpose. For example if user made multiple delegations for the same spo and with the same partner-chain-reward-address, then cancelling delegation will remove one of them. If user want's to remove all delegations, then they have to run this command multiple times. This might not be ideal for experienced users, who'd like to explicitely state which utxo they want to remove, but it simplifies the experience for 95% of use cases.

### 4.3 Security
This approach is secure. The MinotaurStake Token can never leave it's validator address. When it's minted it has to be sent there, and when the utxo from validator address is spent, then the transaction has to burn the MinotaurToken. This means that the only way to delegate funds is to mint the token, sign the transaction with correct stake key hash and send the token to the correct address.

### 4.4 Scalability
This approach should scale as good, as regular cardano trasncations do. There is no single point of congestion, that would slow down the system.

## 5. Recommendations for Modification
- Implement version control for delegations to facilitate updates and modifications in the delegation criteria without disrupting existing delegations.
- Refine the existing PostgreSQL queries to enhance performance. Consider breaking the query into more manageable subqueries that can be executed sequentially or in parallel, reducing the overall execution time. Leveraging database indexing strategies or incorporating a more specialized query optimization tool may also prove beneficial.
- Introduce mechanisms to restrict users from making multiple active delegations with the same stake key hash to the same or different stake pools. This can be enforced at the transaction level or through more sophisticated on-chain logic, thereby simplifying the stake distribution query phase and improving overall system clarity and effectiveness.
- Establish a clear governance framework that can oversee updates to the delegation mechanism. This framework could include stakeholders from various parties who are invested in the stability and efficiency of the system, thus ensuring any changes are made judiciously and inclusively.

## 6. Conclusion
The CNA Delegation Analysis mechanism effectively manages decentralized stake distribution, but there is room for improvement in performance and user experience. The recommended modifications aim to streamline operations, enhance security, and ensure the system's adaptability to future needs. By implementing these changes, the mechanism can become more efficient and user-friendly, supporting its continued relevance in a dynamic environment.
