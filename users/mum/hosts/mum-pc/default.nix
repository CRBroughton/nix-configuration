{ ... }:

{
  imports = [
    ../../common.nix
    ../../vm-testing.nix
    ./hardware.nix
  ];

  networking.networkmanager.enable = true;

  modules.gnome.enable = true;
  modules.flatpak.enable = true;
  modules.gaming.enable = true;
}
