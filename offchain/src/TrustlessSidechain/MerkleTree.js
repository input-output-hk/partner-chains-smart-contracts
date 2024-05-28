import Blake2 from "blakejs";

export function blake2b256Hash(bytesToHash) {
  return Blake2.blake2b(bytesToHash, null, 32);
}