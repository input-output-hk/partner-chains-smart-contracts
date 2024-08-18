{ system ? builtins.currentSystem }:
let
  pins = import ../npins;
  haskellNix = import pins."haskell.nix" { };
  iohkNix = import pins."iohk-nix" { };
  purescript = import pins."purescript-overlay";
  cardanoNode = import pins."cardano-node" { };
  npmLockToNix = import pins."npmlock2nix" { };
  spagoToNix = import pins."spago2nix" { };

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

      # purescript-overlay: provides purescript related packages for the
      # offchain code including purs and spago.
      # NOTE: The overlay does not provide `spago` for `darwin-aarch64` so
      # we are instead using the one provided in nixpkgs via `haskellPackages`
      purescriptOverlay = [ purescript.overlays.default ];

      # cardano-node: We need cardano-node for running the integration tests
      # in offchain
      cardanoNodeOverlay = [ (self: super: { inherit (cardanoNode) cardano-node cardano-cli cardano-testnet; }) ];

      # kupo: We also need kupo for running integration tests
      kupoOverlay = [ (self: super: { kupo = super.callPackage ./packages/kupo.nix { }; }) ];

      # ogmios: Needed for integration testing
      ogmiosOverlay = [ (self: super: { ogmios = super.callPackage ./packages/ogmios.nix { }; }) ];

      # npmlock2nix: used to manage the npm dependencies in the offchain code
      npmlockOverlay = [ (self: super: { inherit npmLockToNix; }) ];

      # spago2nix: required for purescript/spago package integration
      spagoToNixOverlay = [ (self: super: { spago2nix = super.callPackage ./packages/spago2nix.nix { }; }) ];

    in
    chapOverlay
    ++ haskellNixOverlays
    ++ iohkNixCryptoOverlay
    ++ iohkNixOverlays
    ++ haskellNixMapping
    ++ purescriptOverlay
    ++ [ (self: super: { spago = super.haskellPackages.spago; }) ]
    ++ cardanoNodeOverlay
    ++ kupoOverlay
    ++ ogmiosOverlay
    ++ npmlockOverlay
    ++ spagoToNixOverlay;

  config = { allowUnfree = true; };
in
import pins.nixpkgs { inherit system overlays config; }
