var script = 'Scripts/FUELMintingPolicy.plutus';
if (typeof BROWSER_RUNTIME !== 'undefined' && BROWSER_RUNTIME) {
    script = require(script);
} else {
    const fs = require('fs');
    const path = require('path');
    script = fs.readFileSync(
        path.resolve(__dirname, '../../' + script),
        'utf8'
    );
}

exports.fuelMintingPolicy = script;
