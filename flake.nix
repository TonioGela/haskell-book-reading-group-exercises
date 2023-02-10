{
  description = ''
    Personal (partial) solutions to "Haskell programming from First Principles"
  '';

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    flake-root.url = "github:srid/flake-root";
    mission-control.url = "github:Platonic-Systems/mission-control";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
        inputs.mission-control.flakeModule
      ];
      perSystem = {
        self',
        lib,
        config,
        pkgs,
        ...
      }: {
        haskellProjects.main = {
          packages = {
            haskell-book.root = ./.;
          };
          devShell = {
            hlsCheck.enable = true;
          };
        };

        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs = {
            alejandra = {
              enable = true;
              package = pkgs.alejandra;
            };
            cabal-fmt.enable = true;
            ormolu = {
              enable = true;
              package = pkgs.haskellPackages.fourmolu;
            };
            hlint.enable = true;
          };
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        mission-control.scripts = {
          docs = {
            description = "Start Hoogle server for project dependencies";
            exec = ''
              echo http://127.0.0.1:8888
              hoogle serve -p 8888 --local
            '';
          };
          repl = {
            description = "Start the cabal repl";
            exec = ''
              cabal repl "lib:chapter-$1"
            '';
          };
          fmt = {
            description = "Format the source tree";
            exec = "${lib.getExe config.treefmt.build.wrapper}";
          };
        };

        packages.default = self'.packages.main-haskell-book;

        devShells.default =
          config.mission-control.installToDevShell self'.devShells.main;
      };
    };
}
