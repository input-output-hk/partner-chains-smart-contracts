var script;
if (typeof BROWSER_RUNTIME !== 'undefined' && BROWSER_RUNTIME) {
    script = require('Scripts/FUELMintingPolicy.plutus');
} else {
    const fs = require('fs');
    const path = require('path');
    script = fs.readFileSync(
        path.resolve(__dirname, '../../Scripts/FUELMintingPolicy.plutus'),
        'utf8'
    );
}

exports.fuelMintingPolicy = script;
