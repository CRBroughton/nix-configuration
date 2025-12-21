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
    cp "$TEMP_DIR/configs"/*.ovpn "$CONFIG_DIR/" 2>/dev/null || true
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

  # Helper script to manually download/update configurations
  home.file.".local/bin/pia-download-configs" = {
    text = ''
      #!/usr/bin/env bash
      # Download PIA OpenVPN configuration files

      set -e

      CONFIG_DIR="$HOME/.config/pia-vpn"
      TEMP_DIR=$(mktemp -d)

      echo "Private Internet Access - Configuration Downloader"
      echo "=================================================="
      echo ""

      # Create config directory if it doesn't exist
      mkdir -p "$CONFIG_DIR"

      # Download the configuration files
      echo "Downloading PIA OpenVPN configurations..."
      ${pkgs.curl}/bin/curl -L "https://www.privateinternetaccess.com/openvpn/openvpn-strong.zip" -o "$TEMP_DIR/pia-configs.zip"

      # Extract configurations
      echo "Extracting configuration files..."
      ${pkgs.unzip}/bin/unzip -q "$TEMP_DIR/pia-configs.zip" -d "$TEMP_DIR/configs"

      # Copy .ovpn files to config directory
      echo "Installing configuration files to $CONFIG_DIR..."
      cp "$TEMP_DIR/configs"/*.ovpn "$CONFIG_DIR/" 2>/dev/null || true
      cp "$TEMP_DIR/configs"/*.crt "$CONFIG_DIR/" 2>/dev/null || true
      cp "$TEMP_DIR/configs"/*.pem "$CONFIG_DIR/" 2>/dev/null || true

      # Cleanup
      rm -rf "$TEMP_DIR"

      # Count configurations
      CONFIG_COUNT=$(find "$CONFIG_DIR" -name "*.ovpn" | wc -l)

      echo ""
      echo "✓ Successfully downloaded $CONFIG_COUNT VPN configurations"
      echo ""
      echo "Next steps:"
      echo "1. Open GNOME Settings → Network → VPN → Add VPN (+ button)"
      echo "2. Select 'Import from file...'"
      echo "3. Choose a configuration file from: $CONFIG_DIR"
      echo "4. Enter your PIA username and password"
      echo ""
      echo "Available regions:"
      find "$CONFIG_DIR" -name "*.ovpn" -exec basename {} .ovpn \; | sort | column
      echo ""
    '';
    executable = true;
  };

  # Helper script to list available configurations
  home.file.".local/bin/pia-list-configs" = {
    text = ''
      #!/usr/bin/env bash
      # List available PIA VPN configurations

      CONFIG_DIR="$HOME/.config/pia-vpn"

      if [ ! -d "$CONFIG_DIR" ]; then
        echo "Configuration directory not found: $CONFIG_DIR"
        echo "Configurations should download automatically on home-manager switch."
        exit 1
      fi

      CONFIG_COUNT=$(find "$CONFIG_DIR" -name "*.ovpn" | wc -l)

      if [ "$CONFIG_COUNT" -eq 0 ]; then
        echo "No VPN configurations found in $CONFIG_DIR"
        echo "Run 'home-manager switch' or 'pia-download-configs' to download."
        exit 1
      fi

      echo "Available PIA VPN configurations ($CONFIG_COUNT):"
      echo "================================================"
      echo ""
      find "$CONFIG_DIR" -name "*.ovpn" -exec basename {} .ovpn \; | sort | nl
      echo ""
      echo "To import a configuration:"
      echo "  GNOME Settings → Network → VPN → Add VPN → Import from file"
      echo "  Select file from: $CONFIG_DIR"
    '';
    executable = true;
  };
}