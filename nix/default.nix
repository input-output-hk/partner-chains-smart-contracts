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

      # purescript-overlay: provides purescript related packages for the
      # offchain code including purs and spago.
      # NOTE: The overlay does not provide `spago` for `darwin-aarch64` so
      # we are instead using the one provided in nixpkgs via `haskellPackages`
      purescriptOverlay = [ purescript.overlays.default ];

    in
    haskellNixOverlays
    ++ iohkNixCryptoOverlay
    ++ iohkNixOverlays
    ++ purescriptOverlay
    ++ [
      (self: super: {

        # In order to actually apply the changes provided by iohk-nix
        # we need to modify haskell.nix overwriting the attribute set
        # with the altered crypto libraries
        haskell-nix = super.haskell-nix // {
          extraPkgconfigMappings = super.haskell-nix.extraPkgconfigMappings or { } // {
            "libblst" = [ "libblst" ];
            "libsodium" = [ "libsodium-vrf" ];
          };
        };

        CHaP = pins."cardano-haskell-packages";

        cardano-node = cardanoNode.cardano-node;
        cardano-cli = cardanoNode.cardano-cli;
        cardano-testnet = cardanoNode.cardano-testnet;

        kupo = super.callPackage ./packages/kupo.nix { };
        ogmios = super.callPackage ./packages/ogmios.nix { };
        npmlockToNix = npmLockToNix;
        spago = super.haskellPackages.spago;
        spago2nix = super.callPackage ./packages/spago2nix.nix { };
      })
    ];

  config = { allowUnfree = true; };
in
import pins.nixpkgs { inherit system overlays config; }
