{ haskell, emacs, stdenv } :
  stdenv.mkDerivation {
    name = "haskell-interactive-environment";
    src = ./.;
    buildInputs = [
      (haskell.packages.ghc7102.ghcWithHoogle (h : with h; [
      #(haskellPackages.ghcWithHoogle (h : with h; [
        cabal-install stack extensible-effects hint mtl stm gtk wai warp threepenny-gui ]))
      # neovim
      emacs
      ];

    shellHook =
    ''
      # There is some messy business with locating ssl certificates in Nixos:
      export GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt
      export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

      echo "Hello Nix-Shell~"

      echo "Sourcing ghc-variables (for hdevtools and the like)..."
      if type >/dev/null 2>&1 -p ghc; then
        eval "$(egrep ^export "$(type -p ghc)")"
      fi
    '';
  }
