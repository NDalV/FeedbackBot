{
  description = "Adfox overlay";

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.flake-parts.flakeModules.easyOverlay
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.haskell-flake.flakeModule
        ./nix/backend.nix
        ./nix/docs.nix
      ];

      perSystem = {
        pkgs,
        config,
        ...
      }: {
        pre-commit.settings.hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          statix.enable = true;
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.devShells.docs
            config.pre-commit.devShell
          ];
          packages = with pkgs; [
            bashInteractive
          ];
          CA_SSL_LOCATION = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          APP_PORT = 3331;
        };
      };

      flake = {
        nixosModules.default = ./nix/nixos-module.nix;
        hydraJobs.x86_64-linux = {
          inherit (self.outputs.packages.x86_64-linux) default image;
          devShell-default = self.outputs.devShells.x86_64-linux.default;
        };
      };
    };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";

    all-cabal-hashes = {
      url = "github:commercialhaskell/all-cabal-hashes/hackage";
      flake = false;
    };
  };

  nixConfig = {
    allow-import-from-derivation = true;
    extra-substituters = [
      "https://pre-commit-hooks.cachix.org"
    ];
    extra-trusted-substituters = [
      "https://pre-commit-hooks.cachix.org"
    ];
    extra-trusted-public-keys = [
      "pre-commit-hooks.cachix.org-1:Pkk3Panw5AW24TOv6kz3PvLhlH8puAsJTBbOPmBo7Rc="
    ];
  };
}
