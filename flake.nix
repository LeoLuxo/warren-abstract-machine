{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Used to build the project by parsing the cargo dependencies
    naersk = {
      url = "github:nix-community/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Used to generate/get a specific rust toolchain to use with naersk
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      naersk,
      fenix,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = (import nixpkgs) {
          inherit system;
        };

        rustToolchain =
          with fenix.packages.${system};
          fromToolchainFile {
            dir = ./.;
            sha256 = "sha256-yMuSb5eQPO/bHv+Bcf/US8LVMbf/G/0MSfiPwBhiPpk=";
          };
      in
      rec {
        # For `nix build` & `nix run`
        defaultPackage =
          (naersk.lib.${system}.override {
            cargo = rustToolchain;
            rustc = rustToolchain;
          }).buildPackage
            {
              src = ./.;
            };

        # For `nix develop` or `direnv allow`
        devShell = pkgs.mkShell {
          inputsFrom = [ defaultPackage ];
          buildInputs = with pkgs; [
            rustToolchain

            pre-commit
            pkg-config
            libiconv

            # rustup
            rust-analyzer
            rustPackages.clippy
          ];

          # nativeBuildInputs = [ pkgs.pkg-config ];
          # RUST_SRC_PATH = pkgs.rustPlatform.rustLibSrc;
        };
      }
    );
}
