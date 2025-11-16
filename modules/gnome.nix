{ pkgs, lib, config, ... }:

{
  # Install GNOME extensions via gnome-extensions install command
  home.activation.installGnomeExtensions = lib.hm.dag.entryAfter ["writeBoundary"] ''
    # Function to install an extension
    install_extension() {
      local EXTENSION_UUID=$1
      local EXTENSION_URL=$2
      local EXTENSION_NAME=$3

      echo "Checking for $EXTENSION_NAME..."

      # Check if extension is already installed
      GNOME_EXTENSIONS_CMD="${pkgs.gnome-shell}/bin/gnome-extensions"
      if ! $GNOME_EXTENSIONS_CMD list 2>/dev/null | grep -q "$EXTENSION_UUID"; then
        echo "Installing $EXTENSION_NAME from $EXTENSION_URL..."

        # Try using gnome-extensions install
        if $GNOME_EXTENSIONS_CMD install "$EXTENSION_URL" 2>&1; then
          echo "Successfully installed $EXTENSION_NAME"
        else
          echo "gnome-extensions install failed, trying manual installation..."

          # Manual installation fallback
          TEMP_DIR=$(mktemp -d)
          cd "$TEMP_DIR"

          if ${pkgs.curl}/bin/curl -L -o extension.zip "$EXTENSION_URL"; then
            ${pkgs.unzip}/bin/unzip -q extension.zip
            mkdir -p "$HOME/.local/share/gnome-shell/extensions/$EXTENSION_UUID"
            cp -r * "$HOME/.local/share/gnome-shell/extensions/$EXTENSION_UUID/"
            echo "Manually installed $EXTENSION_NAME"
          fi

          cd - > /dev/null
          rm -rf "$TEMP_DIR"
        fi

        # Compile schemas if they exist
        EXTENSION_DIR="$HOME/.local/share/gnome-shell/extensions/$EXTENSION_UUID"
        if [ -d "$EXTENSION_DIR/schemas" ]; then
          echo "Compiling schemas for $EXTENSION_NAME..."
          ${pkgs.glib}/bin/glib-compile-schemas "$EXTENSION_DIR/schemas/" 2>/dev/null || true
        fi
      else
        echo "$EXTENSION_NAME already installed"
      fi
    }

    # Install Tiling Shell extension v61 (compatible with GNOME Shell 49)
    install_extension \
      "tilingshell@ferrarodomenico.com" \
      "https://extensions.gnome.org/extension-data/tilingshellferrarodomenico.com.v61.shell-extension.zip" \
      "Tiling Shell"

    # Install Blur my Shell extension
    install_extension \
      "blur-my-shell@aunetx" \
      "https://extensions.gnome.org/extension-data/blur-my-shellaunetx.v70.shell-extension.zip" \
      "Blur my Shell"

    # Install Caffeine extension
    install_extension \
      "caffeine@patapon.info" \
      "https://extensions.gnome.org/extension-data/caffeinepatapon.info.v58.shell-extension.zip" \
      "Caffeine"
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
        "blur-my-shell@aunetx"
        "caffeine@patapon.info"
      ];
    };
  };
}
