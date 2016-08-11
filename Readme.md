# Hacking on HIE / Getting a nix-shell

To get a functioning nix shell, issue initially:

    $ NIX_PATH=nixpkgs=/path/to/nixpkgs . ./nix/shell.sh
    $ hie-shell-build
    $ hie-shell

On subsequent invocations:

    $ NIX_PATH=nixpkgs=/path/to/nixpkgs . ./nix/shell.sh
    $ hie-shell

`hie-shell-build` builds the derivation specified in `nix/shell.nix`, and places
a nix store gc root in the `nix/` folder. Derivation files are numbered, and
`hie-shell` issued without extra arguments defaults to use the latest.