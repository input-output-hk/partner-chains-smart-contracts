const { spawnSync } = require("node:child_process");
const { Buffer } = require("node:Buffer");

// TODO: perhaps we can do an async version that doesn't block the event loop
// later...
exports.spawnSyncImpl = function (command) {
  return function(args) {
    return function(options) {
      return spawnSync(command, args, options);
    };
  };
};

exports.toBufferImpl = function (uint8array) {
  return Buffer.from(uint8array.buffer);
};

exports.fromBufferImpl = function (buf) {
  return new Uint8Array(buf.buffer, buf.byteOffset, buf.length/ Uint8Array.BYTES_PER_ELEMENT);
};

exports.isNullImpl = function(val) {
  return val === null;
};

exports.id = function (n) {
  return n;
};
