# ICS Authority Token Minting Policy

## Overview

The ICS Authority Token minting policy is a governance-controlled smart contract that allows for the minting and burning of tokens only when transactions are signed by the designated governance authority.

## Purpose

This minting policy provides a secure way to control the issuance and destruction of ICS Authority Tokens within the partner chains ecosystem, ensuring that only authorized governance operations can modify the token supply.

## Functionality

### Supported Operations

1. **Minting** - Create new ICS Authority Tokens
2. **Burning** - Destroy existing ICS Authority Tokens

### Governance Authority Validation

Both minting and burning operations require governance authority approval through the versioning system. The policy uses the `approvedByGovernance` function to verify that transactions are signed by the designated governance authority.

## Implementation Details

### Data Types

The policy uses the `ICSAuthorityTokenRedeemer` data type to distinguish between different operations:

```haskell
data ICSAuthorityTokenRedeemer
  = ICSAuthorityTokenMint  -- For minting operations
  | ICSAuthorityTokenBurn  -- For burning operations
```

### Error Codes

- `ERROR-ICS-AUTHORITY-TOKEN-POLICY-01`: Transaction not signed by governance authority (mint operation)
- `ERROR-ICS-AUTHORITY-TOKEN-POLICY-02`: Transaction not signed by governance authority (burn operation)  
- `ERROR-ICS-AUTHORITY-TOKEN-POLICY-03`: Wrong ScriptContext (should never happen)

### Policy Parameters

The minting policy requires:
- Genesis UTXO (for initialization)
- Version Oracle Configuration (for governance validation)
- Redeemer (mint or burn operation)
- Script Context (transaction context)

## Usage

### Minting Tokens

To mint ICS Authority Tokens, create a transaction with:
1. `ICSAuthorityTokenMint` redeemer
2. Governance authority signature (validated through version oracle)
3. Minting script context with the ICS Authority Token currency symbol

### Burning Tokens

To burn ICS Authority Tokens, create a transaction with:
1. `ICSAuthorityTokenBurn` redeemer  
2. Governance authority signature (validated through version oracle)
3. Minting script context with negative token amounts

## Security Considerations

- Only governance authority can mint or burn tokens
- The policy validates governance signatures through the versioning oracle system
- All operations require proper script context validation
- No tokens can be created or destroyed without proper authorization

## Integration

The ICS Authority Token policy integrates with the existing partner chains governance framework:

- Uses `TrustlessSidechain.Versioning.approvedByGovernance` for authorization
- Follows the same patterns as other governance-controlled policies in the repository
- Compatible with the existing versioning and script caching systems

## File Structure

- **Policy Implementation**: `onchain/src/TrustlessSidechain/ICSAuthorityToken.hs`
- **Data Types**: `onchain/src/TrustlessSidechain/Types.hs` (ICSAuthorityTokenRedeemer)
- **Tests**: `onchain/test/script-spec/ICSAuthorityToken.hs`