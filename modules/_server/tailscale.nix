# Tailscale - VPN service for servers (headless, no systray)
{ config, lib, ... }:

let cfg = config.server.tailscale; in
{
  options.server.tailscale = {
    enable = lib.mkEnableOption "Tailscale VPN (headless server mode)";
  };

  config = lib.mkIf cfg.enable {
    services.tailscale.enable = true;
    networking.firewall.trustedInterfaces = [ "tailscale0" ];
  };
}
