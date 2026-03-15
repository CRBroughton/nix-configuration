# Tailscale - VPN service for servers (headless, no systray)
{ config, lib, ... }:

let
  cfg = config.modules.server.tailscale;
in
{
  options.modules.server.tailscale = {
    enable = lib.mkEnableOption "Tailscale VPN (headless server mode)";
  };

  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;
    networking.firewall.trustedInterfaces = [ "tailscale0" ];
  };
}
