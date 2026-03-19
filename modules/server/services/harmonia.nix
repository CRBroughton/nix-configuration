# Harmonia - Nix binary cache server
{ config, lib, ... }:

let
  cfg = config.modules.server.services.harmonia;
in
{
  options.modules.server.services.harmonia = {
    enable = lib.mkEnableOption "Harmonia Nix binary cache server";
  };

  config = lib.mkIf cfg.enable {
    services.harmonia = {
      enable = true;
      signKeyPaths = [ "/etc/harmonia/signing-key.secret" ];
      settings.bind = "[::]:5000";
    };

    # Only expose on Tailscale interface, not the public internet
    networking.firewall.interfaces."tailscale0".allowedTCPPorts = [ 5000 ];
  };
}
