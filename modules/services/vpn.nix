{ config, pkgs, ... }:

let
  # Download PIA OpenVPN configs
  piaConfigs = pkgs.fetchzip {
    url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
    sha256 = "sha256-mhXV2CF8G7dAkXY9KI7g2qSOlxUzyqgJlidL1At46GU=";
    stripRoot = false;
  };
in
{
  # NetworkManager VPN plugins
  networking.networkmanager = {
    enable = true;
    plugins = with pkgs; [
      networkmanager-openvpn
    ];
  };

  # Make PIA configs available system-wide
  environment.etc."openvpn/pia".source = piaConfigs;

  # Helper script to import PIA configs
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "pia-import" ''
      echo "PIA OpenVPN configs are at: /etc/openvpn/pia/"
      echo ""
      echo "To import a config into NetworkManager:"
      echo "  nmcli connection import type openvpn file /etc/openvpn/pia/<region>.ovpn"
      echo ""
      echo "Available regions:"
      ls /etc/openvpn/pia/*.ovpn | xargs -n1 basename | sed 's/.ovpn//'
    '')
  ];
}
