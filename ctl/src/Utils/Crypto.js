let secp = require("secp256k1");
let crypto = require("crypto");

exports.verifyEcdsaSecp256k1Signature = ecdsa_pub_key => data => ecdsa_der_sig =>
  secp.ecdsaVerify(secp.signatureImport(ecdsa_der_sig), data, ecdsa_pub_key);

// Signature must be serialized in DER format.
exports.sign = data => ecdsa_priv_key =>
  secp.signatureExport(secp.ecdsaSign(data, ecdsa_priv_key).signature);

// rawSerialiseDSIGN for private keys is just hex decoding them, so we need to do nothing here
exports.generateRandomPrivateKey = () => {
  let priv_key;
  do {
    priv_key = crypto.randomBytes(32);
  }
  while (!secp.privateKeyVerify(priv_key));
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
exports.toPubKeyUnsafe = ecdsa_priv_key =>
  secp.publicKeyCreate(ecdsa_priv_key, /*compressed =*/ true);
