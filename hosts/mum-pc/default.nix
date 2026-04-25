{ ... }:

{
  imports = [
    ../../users/mum/vm-testing.nix
    ./hardware.nix
  ];

  networking.networkmanager.enable = true;

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
