let secp = require("secp256k1");
let { randomBytes } = require("crypto");

exports.verifyEcdsaSecp256k1Signature = ecdsa_key => data => ecdsa_sig =>
  secp.ecdsaVerify(ecdsa_sig, data, ecdsa_key);

exports.sign = ecdsa_priv_key => data =>
  secp.ecdsaSign(data, ecdsa_priv_key).signature;

exports.generateRandomPrivateKey = () => {
  let priv_key;
  do {
    priv_key = randomBytes(32);
  }
  while (!secp.privateKeyVerify(priv_key));
  return priv_key;
};

exports.toPubKeyUnsafe = secp.publicKeyCreate;