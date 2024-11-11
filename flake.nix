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
      devShells = forEachSupportedSystem ({ pkgs }: {

        default = pkgs.mkShell {
          packages = with pkgs; [
            jq
            cabal-install
            haskell-language-server
            haskellPackages.HUnit
            haskellPackages.scotty
            create-react-app
            nodejs_22
            python312
            (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
              haskellPackages.HUnit
              haskellPackages.scotty
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
