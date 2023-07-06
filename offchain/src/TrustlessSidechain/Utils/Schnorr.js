let secp = require("@noble/secp256k1");
let { sha256 } = require("@noble/hashes/sha256");

// See the examples in the README:
// https://github.com/paulmillr/noble-secp256k1/tree/1.7.1
// Mostly straightforward wrappers of this.

exports.js_randomPrivateKey = () => {
  const privateKey = secp.utils.randomPrivateKey();
  return privateKey;
};

exports.js_getPublicKey = (privateKey) => {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrgetpublickeyprivatekey
  const publicKey = secp.schnorr.getPublicKey(privateKey);
  return publicKey;
};

exports.js_sign = (msg) => (priv) =>  {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrsignmessage-privatekey
  secp.utils.sha256Sync = (...msgs) => sha256(secp.utils.concatBytes(...msgs));

  const signature = secp.schnorr.signSync(msg, priv);
  return signature;
};

exports.js_verify = (signature) => (message) =>  (publicKey) => {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrverifysignature-message-publickey
  const isValid = secp.schnorr.verify(signature, message, publicKey);
  return isValid;
};
