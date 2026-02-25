# Desktop profile - GNOME with audio, fonts, and common desktop services
{ config, pkgs, ... }:

{
  imports = [
    ../modules/nixos/desktop/gnome.nix
    ../modules/nixos/services/printing.nix
    ../modules/nixos/services/flatpak.nix
  ];

  # Firmware updates
  services.fwupd.enable = true;

  # Networking
  networking.networkmanager.enable = true;
}
