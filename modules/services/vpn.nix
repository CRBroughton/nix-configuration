# VPN - PIA OpenVPN via NetworkManager
{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.vpn;
  piaConfigs = pkgs.fetchzip {
    url = "https://www.privateinternetaccess.com/openvpn/openvpn.zip";
    sha256 = "sha256-mhXV2CF8G7dAkXY9KI7g2qSOlxUzyqgJlidL1At46GU=";
    stripRoot = false;
  };
in
{
  options.services.vpn = {
    enable = lib.mkEnableOption "PIA OpenVPN configs and NetworkManager plugin";
  };

  config = lib.mkIf cfg.enable {
    networking.networkmanager.plugins = with pkgs; [
      networkmanager-openvpn
    ];

    environment.etc."openvpn/pia".source = piaConfigs;

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
  };
}
