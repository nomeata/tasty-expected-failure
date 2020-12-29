{
  description = "Flake to build the tasty-expected-failure haskell package";
  inputs = {
    nixpkgs = { url = "github:nixos/nixpkgs/nixpkgs-unstable"; };
    levers = {
      url = "/home/kquick/Projects/levers";
      inputs.nixpkgs.follows = "nixpkgs";
      };
    tasty-expected-failure-src = {
      url = "github:nomeata/tasty-expected-failure";
      };
    tasty-src = {
      url = "github:feuerbach/tasty/v1.4.0";
      };
    };
  outputs = { self, nixpkgs, levers, tasty-src, tasty-expected-failure-src }:
    rec {
      defaultPackage = levers.eachSystem (s:
        self.packages.${s}.tasty-expected-failure.default);
      devShell = levers.eachSystem (s:
        defaultPackage.${s}.env);
      packages = levers.eachSystem (system:
        let
          mkHaskell = levers.mkHaskellPkg { inherit nixpkgs system; };
          pkgs = import nixpkgs { inherit system; };
        in rec {
          tasty-expected-failure = mkHaskell "tasty-expected-failure" tasty-expected-failure-src {
            inherit tasty;
          };
          tasty = mkHaskell "tasty" tasty-src {};
          });
      };
  }
