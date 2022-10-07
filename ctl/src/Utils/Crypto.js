let secp = require("secp256k1");
let { randomBytes } = require("crypto");

exports.verifyEcdsaSecp256k1Signature = ecdsa_pub_key => data => ecdsa_sig =>
  secp.ecdsaVerify(ecdsa_sig, data, ecdsa_pub_key);

exports.sign = ecdsa_priv_key => data =>
  secp.ecdsaSign(data, ecdsa_priv_key).signature;

// rawSerialiseDSIGN for private keys is just hex decoding them, so we need to do nothing here
exports.generateRandomPrivateKey = () => {
  let priv_key;
  do {
    priv_key = randomBytes(32);
  }
  while (!secp.privateKeyVerify(priv_key));
  return priv_key;
};

// rawSerialiseDSIGN for public keys format:
// - 64 bytes, not 65 bytes, and not equal to decompressed pubkey from here (e.g. minus an extra byte)
// - both the cardano version and this version call to secp256k1_ec_pubkey_serialize c function
// - cardano version: https://github.com/input-output-hk/cardano-base/blob/46cd4c97cff9f1f0a0da976aa9e32bd2899c85ee/cardano-crypto-class/src/Cardano/Crypto/DSIGN/EcdsaSecp256k1.hs#L212-L232
// - this version: https://github.com/cryptocoinjs/secp256k1-node/blob/024fbdad3fb64499e8db8b35246c2f0a36afa8c8/src/secp256k1.cc#L139-L147
//                 https://github.com/cryptocoinjs/secp256k1-node/blob/024fbdad3fb64499e8db8b35246c2f0a36afa8c8/src/secp256k1.cc#L18-L26
// - cardano version claims they only pass the compressed flag to the c function
//   https://github.com/input-output-hk/cardano-base/blob/46cd4c97cff9f1f0a0da976aa9e32bd2899c85ee/cardano-crypto-class/src/Cardano/Crypto/SECP256K1/C.hs#L163
// - I'm not sure how they end up getting their serialized values, this needs more looking into.
// - XXX: in terms of compatibility with the Haskell onchain code, the output here is broken.
exports.toPubKeyUnsafe = ecdsa_priv_key =>
  secp.publicKeyCreate(ecdsa_priv_key, /*compressed =*/ false);
