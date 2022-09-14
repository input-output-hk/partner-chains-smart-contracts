let lib = require("@emurgo/cardano-serialization-lib-nodejs");

exports.publicKeyFromPrivateKeyUnsafe = (bip32private_key) => {
  return bip32private_key.to_public().to_raw_key();
};

exports.sign = (bip32private_key) => (data) => {
  return bip32private_key.to_raw_key().sign(data).to_bytes();
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
