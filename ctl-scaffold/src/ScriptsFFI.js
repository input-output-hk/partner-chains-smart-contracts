/* global BROWSER_RUNTIME */

let inBrowser = typeof BROWSER_RUNTIME !== 'undefined' && BROWSER_RUNTIME;
let fuelMintingPolicy;
let CommitteCandidateValidator;
let MPTRootMintingPolicy;
let MPTRootTokenMintingPolicy;
let MPTRootTokenValidator;
let UpdateCommitteeHash;

if (inBrowser) {
  fuelMintingPolicy          = require('Scripts/FUELMintingPolicy.plutus');
  CommitteCandidateValidator = require("../../Scripts/CommitteCandidateValidator.plutus");
  MPTRootMintingPolicy       = require("../../Scripts/MPTRootMintingPolicy.plutus");
  MPTRootTokenMintingPolicy  = require("../../Scripts/MPTRootTokenMintingPolicy.plutus");
  MPTRootTokenValidator      = require("../../Scripts/MPTRootTokenValidator.plutus");
  UpdateCommitteeHash        = require("../../Scripts/UpdateCommitteeHash.plutus");

} else {
  const fs = require("fs");
  const path = require("path");
  fuelMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/FUELMintingPolicy.plutus"), "utf8");
  CommitteCandidateValidator = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/CommitteCandidateValidator.plutus") , "utf8");
  MPTRootMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootMintingPolicy.plutus") , "utf8");
  MPTRootTokenMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootTokenMintingPolicy.plutus") , "utf8");
  MPTRootTokenValidator = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootTokenValidator.plutus") , "utf8");
  UpdateCommitteeHash = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/UpdateCommitteeHash.plutus") , "utf8");
}
exports.fUELMintingPolicy          = fuelMintingPolicy         
exports.committeCandidateValidator = CommitteCandidateValidator
exports.mPTRootMintingPolicy       = MPTRootMintingPolicy      
exports.mPTRootTokenMintingPolicy  = MPTRootTokenMintingPolicy 
exports.mPTRootTokenValidator      = MPTRootTokenValidator      
exports.updateCommitteeHash        = UpdateCommitteeHash        
