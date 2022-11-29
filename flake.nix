{
  description = "Dev shell for runhs";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.allow-import-from-derivation = true; # cabal2nix uses IFD

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVer = "ghc925";

      makeHaskellOverlay = overlay: final: prev: {
        haskell = prev.haskell // {
          packages = prev.haskell.packages // {
            ${ghcVer} = prev.haskell.packages."${ghcVer}".override (oldArgs: {
              overrides =
                prev.lib.composeExtensions (oldArgs.overrides or (_: _: { }))
                  (overlay prev);
            });
          };
        };
      };

      out = system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ self.overlays.default ];
            config.allowBroken = true;
          };

        in
        {
          packages = rec {
            default = sample;
            sample = pkgs.haskell.packages.${ghcVer}.sample;
          };

          checks = {
            inherit (self.packages.${system}) sample;
          };

          # for debugging
          # inherit pkgs;

          devShells.default =
            let haskellPackages = pkgs.haskell.packages.${ghcVer};
            in
            haskellPackages.shellFor {
              packages = p: [ self.packages.${system}.sample ];
              withHoogle = true;
              buildInputs = with haskellPackages; [
                cabal-install
                fourmolu
                ghcid
                haskdogs
                haskell-language-server
                hasktags
                hlint
                stack
              ] ++ (with pkgs; [
                sqlite
              ]);
              # Change the prompt to show that you are in a devShell
              shellHook = "export PS1='\\e[1;34mdev > \\e[0m'$PS1";
            };
        };
    in
    flake-utils.lib.eachDefaultSystem out // {
      # this stuff is *not* per-system
      overlays = {
        default = makeHaskellOverlay (prev: hfinal: hprev:
          let hlib = prev.haskell.lib; in
          {
            sample = hprev.callCabal2nix "sample" ./. { };

            # here's how to do hacks to the package set
            # don't run the test suite
            # fast-tags = hlib.dontCheck hprev.fast-tags;
            #
            # don't check version bounds
            # friendly = hlib.doJailbreak hprev.friendly;
            #
            # don't do either:
            # ghcid = hlib.dontCheck (hlib.doJailbreak hprev.ghcid);

            # ListLike tests aren't failing, they're hanging indefinitely.
            ListLike = hlib.dontCheck hprev.ListLike;

            # fourmolu's version bounds are too strict for GHC 9.2.5.
            fourmolu = hlib.doJailbreak hprev.fourmolu;

            # Ghcid bin incorrectly depends on Ghcid out on M1.
            # https://github.com/srid/haskell-template/blob/6ecc41c90bc063a4649694533090982bfa5ca47b/flake.nix#L19-L30
            ghcid = hlib.overrideCabal hprev.ghcid (drv: {
                enableSeparateBinOutput = false;
            });
          });
      };
    };
}
