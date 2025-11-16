{ pkgs, ... }:

{
  home.packages = [
    (pkgs.writeShellScriptBin "install-gnome-extensions" ''
      #!/usr/bin/env bash

      echo "Installing GNOME Extensions..."
      echo ""

      # Tiling Shell
      echo "Installing Tiling Shell..."
      gnome-extensions install https://extensions.gnome.org/extension-data/tilingshellferrarodomenico.com.v61.shell-extension.zip

      # Blur my Shell
      echo "Installing Blur my Shell..."
      gnome-extensions install https://extensions.gnome.org/extension-data/blur-my-shellaunetx.v70.shell-extension.zip

      # Caffeine
      echo "Installing Caffeine..."
      gnome-extensions install https://extensions.gnome.org/extension-data/caffeinepatapon.info.v58.shell-extension.zip

      echo ""
      echo "Done! Restart GNOME Shell to load extensions."
      echo "On Wayland: Log out and log back in"
      echo "On X11: Press Alt+F2, type 'r', and press Enter"
    '')
  ];
}
