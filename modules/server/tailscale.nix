# Tailscale - VPN service for servers (headless, no systray)
{ config, ... }:

{
  services.tailscale.enable = true;

  # Trust tailscale interface for firewall
  networking.firewall.trustedInterfaces = [ "tailscale0" ];
}
