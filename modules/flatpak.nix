{ config, pkgs, ... }:

{
  # Flatpak applications - declaratively managed via nix-flatpak
  services.flatpak.enable = true;
  services.flatpak.packages = [
    "com.mattjakeman.ExtensionManager"
    "io.github.equicord.equibop"
    "com.google.Chrome"
    "com.bitwarden.desktop"
    "com.transmissionbt.Transmission"
    "com.heroicgameslauncher.hgl"
    "md.obsidian.Obsidian"
    "org.gnome.Loupe"
    "com.brave.Browser"
    "com.github.neithern.g4music"
    "io.gitlab.news_flash.NewsFlash"
    "io.github.flattool.Warehouse"
    "com.github.tchx84.Flatseal"
    "app.zen_browser.zen"
    "org.virt_manager.virt-manager"
    "com.github.johnfactotum.Foliate"
    "it.mijorus.gearlever"
  ];

  # Fix D-Bus activation for flatpak apps on non-NixOS systems
  # The D-Bus service files point to /run/current-system/sw/bin/flatpak
  # but we need to use the flatpak from our user profile
  home.activation.fixFlatpakDbusServices = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    FLATPAK_BIN="${config.home.profileDirectory}/bin/flatpak"
    DBUS_SERVICES_DIR="${config.home.homeDirectory}/.local/share/dbus-1/services"

    # Find all flatpak D-Bus service files that need fixing
    find "${config.home.homeDirectory}/.local/share/flatpak/exports/share/dbus-1/services" -name "*.service" 2>/dev/null | while read service_file; do
      service_name=$(basename "$service_file")
      override_file="$DBUS_SERVICES_DIR/$service_name"

      # Create override if the service file contains the wrong flatpak path
      if grep -q "/run/current-system/sw/bin/flatpak" "$service_file" 2>/dev/null; then
        mkdir -p "$DBUS_SERVICES_DIR"
        sed "s|/run/current-system/sw/bin/flatpak|$FLATPAK_BIN|g" "$service_file" > "$override_file"
        $DRY_RUN_CMD echo "Fixed D-Bus service: $service_name"
      fi
    done

    # Reload systemd user daemon to pick up the new service files
    $DRY_RUN_CMD systemctl --user daemon-reload || true

    # Restart the D-Bus user session to reload service definitions
    # We use 'restart' instead of 'reload' to ensure changes take effect
    $DRY_RUN_CMD systemctl --user restart dbus.service || true
  '';
}
