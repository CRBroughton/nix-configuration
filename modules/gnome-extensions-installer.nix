{ pkgs, ... }:

# How to add more GNOME extensions:
#
# 1. Find the extension on https://extensions.gnome.org
# 2. Note the extension ID from the URL (e.g., 3210 for compiz-windows-effect)
# 3. Get your GNOME Shell version: gnome-shell --version
# 4. Get the download URL using the API:
#    curl -s "https://extensions.gnome.org/extension-info/?pk=EXTENSION_ID&shell_version=SHELL_VERSION" | \
#      jq -r '"https://extensions.gnome.org" + .download_url'
# 5. Add the extension below following the existing pattern
#
# Example:
#   Extension: https://extensions.gnome.org/extension/3210/compiz-windows-effect/
#   ID: 3210, Shell version: 49
#   Command: curl -s "https://extensions.gnome.org/extension-info/?pk=3210&shell_version=49" | jq -r '"https://extensions.gnome.org" + .download_url'
#   Output: https://extensions.gnome.org/download-extension/compiz-windows-effect@hermes83.github.com.shell-extension.zip?version_tag=65059

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

      # Compiz Windows Effect
      echo "Installing Compiz Windows Effect..."
      gnome-extensions install https://extensions.gnome.org/download-extension/compiz-windows-effect@hermes83.github.com.shell-extension.zip?version_tag=65059

      # AppIndicator and KStatusNotifierItem Support
      echo "Installing AppIndicator Support..."
      gnome-extensions install https://extensions.gnome.org/download-extension/appindicatorsupport@rgcjonas.gmail.com.shell-extension.zip?version_tag=65144

      echo ""
      echo "Done! Restart GNOME Shell to load extensions."
      echo "On Wayland: Log out and log back in"
      echo "On X11: Press Alt+F2, type 'r', and press Enter"
    '')
  ];
}
