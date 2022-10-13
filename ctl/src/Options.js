
exports.parsePubKeyAndSignatureImpl = nothing => just => str => {
  // Regexes tend to be a bit unreadable.. As a EBNF grammar, we're matching:
  //   > pubKeyAndSig
  //   >      -> hexStr [ ':' [hexStr]]
  // where `hexStr` is a a sequence of non empty hex digits of even length (the even
  // length requirement is imposed by 'Contract.Prim.ByteArray.hexToByteArray').
  // i.e., we are parsing a `hexStr` followed optionally by a colon ':', and
  // followed optionally by another non empty `hexStr`.
  const matches = str.match(/^((?:[0-9a-f]{2})+)(?::((?:[0-9a-f]{2})+)?)?$/i);
  if (matches != null) {
    return just(
      { pubKey: matches[1]
        , signature: matches[2] == undefined ? nothing : just(matches[2])
      });
  } else {
    return nothing;
  }
};
