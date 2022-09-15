let lib = require("@emurgo/cardano-serialization-lib-nodejs");

exports.publicKeyFromPrivateKeyUnsafe = (bip32private_key) => {
  return bip32private_key.to_public().to_raw_key();
};

exports.sign = (bip32private_key) => (data) => {
  return bip32private_key.to_raw_key().sign(data).to_bytes();
};

exports.verifyEd25519Signature = (ed25519pub_key) => (data) => (ed25519sig) => {
  const pub_key = lib.PublicKey.from_bytes(ed25519pub_key);
  const sig = lib.Ed25519Signature.from_bytes(ed25519sig);
  return pub_key.verify(data, sig);
};

exports.publicKeyToBytesUnsafe = (public_key) => {
  return public_key.as_bytes();
};

exports.generateRandomBIP32PrivateKeyArrayInt8 = () => {
  return lib.Bip32PrivateKey.generate_ed25519_bip32().to_128_xprv();
};
exports.generateBIP32PrivateKeyFromArray = (array) => {
  return lib.Bip32PrivateKey.from_128_xprv(array);
};
