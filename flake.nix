{
  nixConfig.bash-prompt = "[nix(butler)] ";
  inputs = {
    hspkgs.url =
      "github:podenv/hspkgs/dd4b3e5a7d36211ae4df0270d7553afa3db8f435";
      # "path:///srv/github.com/podenv/hspkgs";
  };
  outputs = { self, hspkgs }:
    let
      pkgs = hspkgs.pkgs;
      haskellExtend = hpFinal: hpPrev: {
        butler = hpPrev.callCabal2nix "butler" self { };
      };
      hsPkgs = pkgs.hspkgs.extend haskellExtend;
    in {
      packages."x86_64-linux".default = pkgs.haskell.lib.justStaticExecutables hsPkgs.butler;
      devShell."x86_64-linux" = hsPkgs.shellFor {
        packages = p: [ p.butler ];
        buildInputs = with pkgs; [
          hpack
          cabal-install
          ghcid
          haskell-language-server
          fourmolu
          hsPkgs.doctest
        ];
      };
    };
}
