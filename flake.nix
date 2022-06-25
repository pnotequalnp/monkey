{
  description = "Monkeylang";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    diagnose = {
      url = "github:mesabloo/diagnose";
      flake = false;
    };
    effectful = {
      url = "github:haskell-effectful/effectful";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, diagnose, effectful }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      inherit (pkgs.lib) composeExtensions pipe;
      inherit (pkgs.haskell.lib.compose) addBuildDepend enableCabalFlag;
      hs = pkgs.haskell.packages.ghc922;
      hs' = hs.override {
        overrides = composeExtensions pkgs.haskell.packageOverrides (final: prev: {
          diagnose = pipe (final.callCabal2nix "diagnose" diagnose { }) [
            (enableCabalFlag "megaparsec-compat")
            (addBuildDepend final.megaparsec)
          ];
          effectful = final.callCabal2nix "effectful" "${effectful}/effectful" { };
          effectful-core = final.callCabal2nix "effectful-core" "${effectful}/effectful-core" { };
          hashable = final.hashable_1_4_0_2;
          parsec = final.parsec_3_1_15_1;
          text = final.text_2_0;
          Cabal = final.Cabal_3_6_3_0;
        });
      };
      monkey = hs'.callCabal2nix "monkey" ./. { };
    in {
      packages.${system} = {
        default = monkey;
        monkey = monkey;
      };

      apps.${system} = rec {
        default = monkey;
        monkey = {
          type = "app";
          program = "${self.packages.${system}.monkey}/bin/monkey";
        };
      };

      devShells.${system} = {
        default = hs.shellFor {
          packages = p: [ monkey ];
          nativeBuildInputs = with hs; [
            cabal-install
            fourmolu
            haskell-language-server
            hlint
          ];
        };
        ci = hs.shellFor {
          packages = p: [ ];
          nativeBuildInputs = with hs; [ cabal-install fourmolu hlint ];
        };
      };
    };
}
