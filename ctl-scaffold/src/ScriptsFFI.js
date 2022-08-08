/* global BROWSER_RUNTIME */

var inBrowser = typeof BROWSER_RUNTIME !== 'undefined' && BROWSER_RUNTIME;

if (inBrowser) {
  const fuelMintingPolicy          = require('Scripts/FUELMintingPolicy.plutus');
  const CommitteCandidateValidator = require("../../Scripts/CommitteCandidateValidator.plutus");
  const MPTRootMintingPolicy       = require("../../Scripts/MPTRootMintingPolicy.plutus");
  const MPTRootTokenMintingPolicy  = require("../../Scripts/MPTRootTokenMintingPolicy.plutus");
  const MPTRootTokenValidator      = require("../../Scripts/MPTRootTokenValidator.plutus");
  const UpdateCommitteeHash        = require("../../Scripts/UpdateCommitteeHash.plutus");

} else {
  const fs = require("fs");
  const path = require("path");
  var fuelMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/FUELMintingPolicy.plutus"), "utf8");
  var CommitteCandidateValidator = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/CommitteCandidateValidator.plutus") , "utf8");
  var MPTRootMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootMintingPolicy.plutus") , "utf8");
  var MPTRootTokenMintingPolicy = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootTokenMintingPolicy.plutus") , "utf8");
  var MPTRootTokenValidator = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/MPTRootTokenValidator.plutus") , "utf8");
  var UpdateCommitteeHash = fs.readFileSync(path.resolve(__dirname
    , "../../Scripts/UpdateCommitteeHash.plutus") , "utf8");
}
exports.fUELMintingPolicy          = fuelMintingPolicy         
exports.committeCandidateValidator = CommitteCandidateValidator
exports.mPTRootMintingPolicy       = MPTRootMintingPolicy      
exports.mPTRootTokenMintingPolicy  = MPTRootTokenMintingPolicy 
exports.mPTRootTokenValidator      = MPTRootTokenValidator      
exports.updateCommitteeHash        = UpdateCommitteeHash        
