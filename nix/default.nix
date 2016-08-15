{ haskell, stdenv, nodePackages } :
  stdenv.mkDerivation {
    name = "haskell-interactive-environment";
    builder = ./trivialbuilder.sh;
    buildInputs = [
      #(haskell.packages.ghc7102.ghcWithHoogle (h : with h; [
      (haskell.packages.ghc7103.ghcWithHoogle (h : with h; [
      #(haskell.packages.ghc801.ghcWithHoogle (h : with h; [
        cabal-install
        stack
        extensible-effects
        diagrams
        compdata
        mtl
        stm
        #gtk
        wai
        warp
        constraints
        /*
        this is only present here because of editor support.
        Intero does not yet support ghcjs, let alone having some open
        files belonging to a ghc project and others belonging to a
        ghcjs project
        */
        reflex
        reflex-dom
        Chart-diagrams
        #diagrams-reflex
        ]))
      (haskell.packages.ghcjs.ghcWithPackages (h : with h; [
        Chart-diagrams
        constraints
        Cabal
        reflex
        compdata
        #diagrams-reflex
        reflex-dom
        ]))
      /* needed for socket.io, which appears to be absent from nixpkgs
      */
      nodePackages.npm
      ];

    shellHook =
    ''
      # There is some messy business with locating ssl certificates in Nixos:
      export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
      export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

      echo "Hello Nix-Shell~"

      echo "Sourcing ghc-variables (for hdevtools, hint, intero, and the like)..."
      if type >/dev/null 2>&1 -p ghc; then
        eval "$(egrep ^export "$(type -p ghc)")"
      fi
      if type >/dev/null 2>&1 -p ghcjs; then
        eval "$(egrep ^export "$(type -p ghcjs)")"
      fi
    '';
  }
