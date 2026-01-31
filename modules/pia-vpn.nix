{ config, pkgs, ... }:

let
  # Script to download PIA configurations
  downloadPiaConfigs = pkgs.writeShellScript "download-pia-configs" ''
    CONFIG_DIR="$HOME/.config/pia-vpn"
    TEMP_DIR=$(mktemp -d)

    # Create config directory if it doesn't exist
    mkdir -p "$CONFIG_DIR"

    # Check if configs already exist
    if [ -n "$(find "$CONFIG_DIR" -name "*.ovpn" -print -quit 2>/dev/null)" ]; then
      echo "PIA VPN configurations already exist in $CONFIG_DIR"
      exit 0
    fi

    echo "Downloading PIA OpenVPN configurations..."
    ${pkgs.curl}/bin/curl -L "https://www.privateinternetaccess.com/openvpn/openvpn-strong.zip" -o "$TEMP_DIR/pia-configs.zip"

    echo "Extracting configuration files..."
    ${pkgs.unzip}/bin/unzip -q "$TEMP_DIR/pia-configs.zip" -d "$TEMP_DIR/configs"

    echo "Installing configuration files to $CONFIG_DIR..."
    echo "Fixing OpenSSL 3.5 compatibility issues..."

    # Fix .ovpn files by removing problematic CRL sections
    for ovpn_file in "$TEMP_DIR/configs"/*.ovpn; do
      if [ -f "$ovpn_file" ]; then
        ${pkgs.gnused}/bin/sed '/<crl-verify>/,/<\/crl-verify>/d' "$ovpn_file" > "$CONFIG_DIR/$(basename "$ovpn_file")"
      fi
    done

    # Copy other files as-is
    cp "$TEMP_DIR/configs"/*.crt "$CONFIG_DIR/" 2>/dev/null || true
    cp "$TEMP_DIR/configs"/*.pem "$CONFIG_DIR/" 2>/dev/null || true

    # Cleanup
    rm -rf "$TEMP_DIR"

    CONFIG_COUNT=$(find "$CONFIG_DIR" -name "*.ovpn" | wc -l)
    echo "✓ Successfully downloaded $CONFIG_COUNT PIA VPN configurations"
    echo ""
    echo "To set up VPN:"
    echo "  GNOME Settings → Network → VPN → Add VPN → Import from file"
    echo "  Select a configuration from: $CONFIG_DIR"
  '';
in
{
  # Private Internet Access VPN configuration
  # Uses Fedora's built-in OpenVPN support via NetworkManager

  # Ensure unzip is available for extracting configs
  home.packages = with pkgs; [
    unzip
  ];

  # Create a directory for VPN configurations
  home.file.".config/pia-vpn/.keep".text = "";

  # Activation script to download configs on home-manager switch
  home.activation.downloadPiaConfigs = config.lib.dag.entryAfter ["writeBoundary"] ''
    $DRY_RUN_CMD ${downloadPiaConfigs}
  '';

  # Auto-connect VPN on login
  systemd.user.services.pia-vpn-autoconnect = {
    Unit = {
      Description = "Auto-connect PIA VPN";
      After = [ "network-online.target" ];
      Wants = [ "network-online.target" ];
    };
    Service = {
      Type = "oneshot";
      ExecStartPre = "${pkgs.coreutils}/bin/sleep 10";
      ExecStart = "${pkgs.networkmanager}/bin/nmcli connection up belgium";
      RemainAfterExit = true;
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}