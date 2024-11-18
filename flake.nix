{
  description = "A Nix-flake-based Haskell development environment";

  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit system; };
      });
    in
    {

      packages = forEachSupportedSystem ({ pkgs }: {
        engine = pkgs.stdenv.mkDerivation {
          name = "engine";
          src = ./engine; 
          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
              haskellPackages.scotty
              haskellPackages.wai-cors
            ]))
          ];
          buildPhase = ''
            ghc Main.hs -o engine
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp engine $out/bin/engine
          '';
        };

        backend = pkgs.stdenv.mkDerivation {
          name = "backend";
          src = ./backend; 
          buildInputs = with pkgs; [
            (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
              haskellPackages.scotty
              haskellPackages.wai-cors
              haskellPackages.websockets
              haskellPackages.postgresql-simple
            ]))
          ];
          buildPhase = ''
            ghc Main.hs -o backend
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp backend $out/bin/backend
          '';
        };
      });

      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            jq
            cabal-install
            haskell-language-server
            haskellPackages.scotty
            haskellPackages.wai-cors
            haskellPackages.websockets
            haskellPackages.postgresql-simple
            create-react-app
            nodejs_22
            go
            python312
            (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
              haskellPackages.scotty
              haskellPackages.postgresql-simple
              haskellPackages.wai-cors
              haskellPackages.websockets
            ]))
          ] ++ (with pkgs.python312Packages; [
            pip
            pygame-ce
            requests
          ]) ++
            (with pkgs.nodePackages; [
              tailwindcss
            ]);
        };
      });
    };
}
