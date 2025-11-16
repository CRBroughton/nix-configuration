{ pkgs, lib, config, ... }:

{
  # Install GNOME extensions via gnome-extensions install command
  home.activation.installGnomeExtensions = lib.hm.dag.entryAfter ["writeBoundary"] ''
    # Install Tiling Shell extension v61 (compatible with GNOME Shell 49)
    EXTENSION_UUID="tilingshell@ferrarodomenico.com"
    EXTENSION_URL="https://extensions.gnome.org/extension-data/tilingshellferrarodomenico.com.v61.shell-extension.zip"

    echo "Checking for Tiling Shell extension..."
    if ! gnome-extensions list 2>/dev/null | grep -q "$EXTENSION_UUID"; then
      echo "Installing Tiling Shell extension from $EXTENSION_URL..."

      # Try using gnome-extensions install
      if gnome-extensions install "$EXTENSION_URL" 2>&1; then
        echo "Successfully installed Tiling Shell extension"
      else
        echo "gnome-extensions install failed, trying manual installation..."

        # Manual installation fallback
        TEMP_DIR=$(mktemp -d)
        cd "$TEMP_DIR"

        if ${pkgs.curl}/bin/curl -L -o extension.zip "$EXTENSION_URL"; then
          ${pkgs.unzip}/bin/unzip -q extension.zip
          mkdir -p "$HOME/.local/share/gnome-shell/extensions/$EXTENSION_UUID"
          cp -r * "$HOME/.local/share/gnome-shell/extensions/$EXTENSION_UUID/"
          echo "Manually installed Tiling Shell extension"
        fi

        cd - > /dev/null
        rm -rf "$TEMP_DIR"
      fi
    else
      echo "Tiling Shell extension already installed"
    fi
  '';

  # Environment variables for GNOME apps to find TLS support
  # home.sessionVariables = {
  #   GIO_EXTRA_MODULES = "${pkgs.glib-networking}/lib/gio/modules";
  #   GST_PLUGIN_SYSTEM_PATH_1_0 = "${pkgs.glib-networking}/lib/gstreamer-1.0";
  # };

  # GNOME dconf settings
  dconf.settings = {
    # Set favorite apps in GNOME dock
    "org/gnome/shell" = {
      favorite-apps = [
        "app.zen_browser.zen.desktop"
        "org.gnome.Nautilus.desktop"
        "io.github.equicord.equibop.desktop"
        "com.bitwarden.desktop.desktop"
        "code.desktop"
        "com.mitchellh.ghostty.desktop"
        "com.ranfdev.DistroShelf.desktop"
        "steam.desktop"
        "net.lutris.Lutris.desktop"
        "com.heroicgameslauncher.hgl.desktop"
        "md.obsidian.Obsidian.desktop"
      ];
      enabled-extensions = [
        "tilingshell@ferrarodomenico.com"
      ];
    };
  };
}
