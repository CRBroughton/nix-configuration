{ pkgs, ... }:

{
  imports = [
    ../../users/moon/vm-testing.nix
    ./hardware.nix
  ];

  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [
    google-chrome
    ckb-next
  ];

  modules.gnome.enable = true;
  modules.flatpak.enable = true;
  modules.gaming.enable = true;
  modules.tailscale.enable = true;
  modules.autoUpgrade.enable = true;
  modules.browsers.zen.enable = true;
  modules.media.enable = true;
}
