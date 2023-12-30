{
  inputs = {
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    flake-parts,
    ...
  } @ inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.pre-commit-hooks.flakeModule
      ];

      systems = [
        "aarch64-darwin"
      ];

      perSystem = {
        config,
        system,
        inputs',
        ...
      }: let
        pkgs = import inputs.nixpkgs {
          inherit system;
        };
      in {
        treefmt.projectRootFile = "flake.nix";
        treefmt.programs = {
          alejandra.enable = true;
          scalafmt.enable = true;
        };

        pre-commit = {
          check.enable = true;
          settings.hooks = {
            treefmt.enable = true;
          };
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [config.pre-commit.devShell];

          packages = [
          ];

          buildInputs = with pkgs; [
            coursier
            jre8_headless
            sbt
            scalafmt
            iconv
            pkgs.darwin.apple_sdk.frameworks.IOKit
          ];

          JAVA_HOME = "${pkgs.jre8_headless}";
        };
      };
    };
}
