let secp = require("secp256k1");
let crypto = require("crypto");

exports.verifyEcdsaSecp256k1Signature =
  (ecdsa_pub_key) => (data) => (ecdsa_der_sig) =>
    secp.ecdsaVerify(ecdsa_der_sig, data, ecdsa_pub_key);

exports.sign = (data) => (ecdsa_priv_key) =>
  secp.ecdsaSign(data, ecdsa_priv_key).signature;

// rawSerialiseDSIGN for private keys is just hex decoding them, so we need to do nothing here
exports.generateRandomPrivateKey = () => {
  let priv_key;
  do {
    priv_key = crypto.randomBytes(32);
  } while (!secp.privateKeyVerify(priv_key));
  return priv_key;
};

// rawSerialiseDSIGN for public keys format:
// - 33-bit compressed pubkey.
// - both the cardano version and this version call to secp256k1_ec_pubkey_serialize c function
// - neither deals directly with the internal state of the pubkeys, especially not onchain
// see:
// - https://github.com/cardano-foundation/CIPs/blob/c5bdd66fe49c19c341499f86cebaa2eef9e90b74/CIP-0049/README.md#specification
// - https://github.com/input-output-hk/cardano-base/blob/737d0c50d10db63ee55f9a49c66da50573088818/cardano-crypto-class/src/Cardano/Crypto/DSIGN/EcdsaSecp256k1.hs#L225
// - https://github.com/input-output-hk/cardano-base/blob/737d0c50d10db63ee55f9a49c66da50573088818/cardano-crypto-class/src/Cardano/Crypto/SECP256K1/C.hs#L157-L164
// - https://github.com/input-output-hk/cardano-base/pull/289
exports.toPubKeyUnsafe = (ecdsa_priv_key) =>
  secp.publicKeyCreate(ecdsa_priv_key, /*compressed =*/ true);

// This verifies if we have a valid public key (for both compresed [33 byte] or
// uncompressed [65 bytes]):
// Tracing through the implementation:
//   - The C function call is here in the js wrapper:
//   https://github.com/cryptocoinjs/secp256k1-node/blob/master/src/secp256k1.cc#L129
//   - The implementation is here:
//   https://github.com/bitcoin-core/secp256k1/blob/5c789dcd7318649c43d89361eaaa07c3bd1c9c57/src/secp256k1.c#L248
//   - And most of the heavy checks happen in this function: https://github.com/bitcoin-core/secp256k1/blob/9a5a87e0f1276e0284446af1172056ea4693737f/src/eckey_impl.h#L17
exports.pubKeyVerify = (publicKey) =>
  secp.publicKeyVerify(publicKey);


// This verifies that the given secret key (as an integer) is non zero and
// lower than the secp256k1 curve's order..
// Tracing through the implementation:
//  - The C function call is here in the js wrapper:
//  https://github.com/cryptocoinjs/secp256k1-node/blob/master/src/secp256k1.cc#L101
//  - Which calls the well documented C function:
//  https://github.com/bitcoin-core/secp256k1/blob/e3f84777eba58ea010e61e02b0d3a65787bc4fd7/include/secp256k1.h#L662-L673
exports.secKeyVerify = (secretKey) =>
  secp.privateKeyVerify(secretKey);
