{ pkgs, ... }:

{
  home.packages = [
    (pkgs.writeShellScriptBin "nix-just" ''
      #!/usr/bin/env bash

      # Global wrapper for justfile in nix-configuration directory
      # Allows running just commands from anywhere

      NIX_CONFIG_DIR="$HOME/nix-configuration"

      if [ ! -d "$NIX_CONFIG_DIR" ]; then
        echo "Error: Nix configuration directory not found at $NIX_CONFIG_DIR"
        exit 1
      fi

      if [ ! -f "$NIX_CONFIG_DIR/justfile" ]; then
        echo "Error: justfile not found at $NIX_CONFIG_DIR/justfile"
        exit 1
      fi

      # Run just with the nix-configuration directory as working directory
      cd "$NIX_CONFIG_DIR" && ${pkgs.just}/bin/just "$@"
    '')
  ];
}