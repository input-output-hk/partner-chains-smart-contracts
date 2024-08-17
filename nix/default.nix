{ system ? builtins.currentSystem }:
let
  pins = import ../npins;
  haskellNix = import pins."haskell.nix" { };
  iohkNix = import pins."iohk-nix" { };

  overlays =
    let
      # iohk-nix: provides overrides on crypto packages that
      # we depend on for the onchain haskell build. The flake
      # is unfortunately not maintained in a way that it can
      # easily be consumed without flakes so we have to do
      # some bootstrapping here providing the inputs ourselves
      # as additional pins.
      iohkNixCryptoOverlay =
        let
          f = builtins.head iohkNix.overlays.crypto;
          iohkNixCryptoInputs = {
            sodium = pins.libsodium // { shortRev = "dbb48cce"; };
            blst = pins.blst // { shortRev = "1l1c905"; };
            secp256k1 = pins.secp256k1 // { shortRev = "acf5c55"; };
          };
        in
        [ (f iohkNixCryptoInputs) ];

      # haskell.nix: provides haskell/nix integration that
      # we use for the onchain code
      haskellNixOverlays = haskellNix.overlays;

      # iohk-nix: provides some more additions to the haskell.nix
      # packaging
      iohkNixOverlays = iohkNix.overlays."haskell-nix-extra";

      # cardano-haskell-packages: provides cardano related haskell pacakges
      chapOverlay = [ (self: super: { CHaP = pins."cardano-haskell-packages"; }) ];

      # In order to actually apply the changes provided by iohk-nix
      # we need to modify haskell.nix overwriting the attribute set
      # with the altered crypto libraries
      haskellNixMapping = [
        (self: super: {
          haskell-nix = super.haskell-nix // {
            extraPkgconfigMappings = super.haskell-nix.extraPkgconfigMappings or { } // {
              "libblst" = [ "libblst" ];
              "libsodium" = [ "libsodium-vrf" ];
            };
          };
        })
      ];
    in
    chapOverlay ++ haskellNixOverlays ++ iohkNixCryptoOverlay ++ iohkNixOverlays ++ haskellNixMapping;

  config = { allowUnfree = true; };
in
import pins.nixpkgs { inherit system overlays config; }
