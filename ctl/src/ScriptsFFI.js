/* global BROWSER_RUNTIME */

let inBrowser = typeof BROWSER_RUNTIME !== "undefined" && BROWSER_RUNTIME;
let fuelMintingPolicy;
let committeeCandidateValidator;
let mptRootTokenMintingPolicy;
let mptRootTokenValidator;
let updateCommitteeHash;

if (inBrowser) {
  fuelMintingPolicy = require("Scripts/FUELMintingPolicy.plutus");
  committeeCandidateValidator = require("../../Scripts/CommitteeCandidateValidator.plutus");
  mptRootTokenMintingPolicy = require("../../Scripts/MPTRootTokenMintingPolicy.plutus");
  mptRootTokenValidator = require("../../Scripts/MPTRootTokenValidator.plutus");
  updateCommitteeHash = require("../../Scripts/UpdateCommitteeHash.plutus");
} else {
  const fs = require("fs");
  const path = require("path");
  fuelMintingPolicy = fs.readFileSync(
    path.resolve(__dirname, "../../Scripts/FUELMintingPolicy.plutus"),
    "utf8"
  );
  committeeCandidateValidator = fs.readFileSync(
    path.resolve(__dirname, "../../Scripts/CommitteeCandidateValidator.plutus"),
    "utf8"
  );
  mptRootTokenMintingPolicy = fs.readFileSync(
    path.resolve(__dirname, "../../Scripts/MPTRootTokenMintingPolicy.plutus"),
    "utf8"
  );
  mptRootTokenValidator = fs.readFileSync(
    path.resolve(__dirname, "../../Scripts/MPTRootTokenValidator.plutus"),
    "utf8"
  );
  updateCommitteeHash = fs.readFileSync(
    path.resolve(__dirname, "../../Scripts/UpdateCommitteeHash.plutus"),
    "utf8"
  );
}
exports.rawFUELMintingPolicy = fuelMintingPolicy;
exports.rawCommitteeCandidateValidator = committeeCandidateValidator;
exports.rawMPTRootTokenMintingPolicy = mptRootTokenMintingPolicy;
exports.rawMPTRootTokenValidator = mptRootTokenValidator;
exports.rawUpdateCommitteeHash = updateCommitteeHash;
