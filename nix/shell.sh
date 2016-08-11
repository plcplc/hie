# usage: dot-source this script.
# The functions herein close over the $NIX_PATH at sourcing time.
# This is useful if you experiment with different nixpkgs repos.


hienixdir=$(dirname $(readlink -fn $0))
nix_path=$NIX_PATH

function hie-shell-next ()
{
  latestMaybe=$(find "$hienixdir"  -name "hie-shell-*.drv"| tail -n 1 | sed -n 's/.*hie-shell-\([0-9]\+\)\.drv/\1/p')

  if [ x$latestMaybe = x ];
  then
      nextPath="$hienixdir/hie-shell-0.drv"
  else
      nextNum=$(( $latestMaybe + 1))
      nextPath="$hienixdir/hie-shell-$nextNum.drv"
  fi
  echo $nextPath
}

function hie-shell ()
{

  if [ "$#" -eq 1 ];
  then
    shellDrv="$hienixdir/hie-shell-$1.drv"
    if [ ! -h $shellDrv ];
    then
      echo "No hie-shell derivation found. Run hie-shell-build."
      return -1
    fi
  else
    shellDrvMaybe=$(find "$hienixdir"  -name "hie-shell-*.drv" | tail -n 1)
    if [ x$shellDrvMaybe = x ];
    then
      echo "No hie-shell derivation found. Run hie-shell-build."
      return -1
    else
      shellDrv=$shellDrvMaybe
    fi
  fi

  echo "Using hie-shell derivation $(basename $shellDrv)"
  nix-shell $shellDrv
}

function hie-shell-build ()
{
  NIX_PATH=$nix_path nix-build \
    --drv-link "$(hie-shell-next)" \
    --no-out-link \
    "$hienixdir/shell.nix"
}
