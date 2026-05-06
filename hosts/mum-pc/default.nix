{ disko, ... }:

{
  imports = [
    ../../users/mum/vm-testing.nix
    ./hardware.nix
    (disko + "/mum-pc.nix")
  ];

  networking.networkmanager.enable = true;
  security.sudo.wheelNeedsPassword = false;

  # Allow wayvnc only on the Tailscale interface
  networking.firewall.interfaces.tailscale0.allowedTCPPorts = [ 5900 ];

  modules.ssh.enable = true;
  modules.gnome.enable = true;
  modules.flatpak.enable = true;
  modules.gaming.enable = true;
  modules.monitoringNode.enable = true;
  modules.tailscale.enable = true;
  modules.autoUpgrade = {
    enable = true;
    dates = "*:0/15"; # every 15 minutes — picks up pushed changes quickly
  };
}
