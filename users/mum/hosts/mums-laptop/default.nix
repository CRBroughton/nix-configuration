{ ... }:

{
  imports = [
    ../../common.nix
    # ../../vm-testing.nix   # uncomment only for VM testing
    ./hardware.nix
  ];

  networking.networkmanager.enable = true;

  modules.gnome.enable = true;
  modules.flatpak.enable = true;
  modules.gaming.enable = true;
}
