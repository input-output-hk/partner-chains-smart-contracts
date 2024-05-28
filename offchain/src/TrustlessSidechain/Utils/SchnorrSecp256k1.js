// let secp = import("@noble/secp256k1");
import * as secp from "@noble/secp256k1";
// let { sha256 } = import("@noble/hashes/sha256");
import {sha256} from "@noble/hashes/sha256";
// See the examples in the README:
// https://github.com/paulmillr/noble-secp256k1/tree/1.7.1
// Mostly straightforward wrappers of this.

export const js_randomPrivateKey = () => {
  return secp.utils.randomPrivateKey();
};

export const js_getPublicKey = (privateKey) => {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrgetpublickeyprivatekey
  return secp.schnorr.getPublicKey(privateKey);
};

export const js_sign = (msg) => (priv) =>  {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrsignmessage-privatekey
  secp.utils.sha256Sync = (...msgs) => sha256(secp.utils.concatBytes(...msgs));

  return secp.schnorr.signSync(msg, priv);
};

export const js_verify = (signature) => (message) =>  (publicKey) => {
  // See: https://github.com/paulmillr/noble-secp256k1/tree/1.7.1#schnorrverifysignature-message-publickey
  return secp.schnorr.verify(signature, message, publicKey);
};
