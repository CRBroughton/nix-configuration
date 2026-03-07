# Graphical desktop profile - base GUI environment
{ config, pkgs, ... }:

{
  imports = [
    ../modules/nixos/desktop/gnome.nix
    ../modules/nixos/services/printing.nix
    ../modules/nixos/services/flatpak/base.nix
    ../modules/nixos/services/ssh.nix
    ../modules/nixos/services/tailscale.nix
    ../modules/nixos/services/vpn.nix
    ../modules/nixos/security.nix
    ../modules/nixos/nix.nix
  ];

  # Firmware updates
  services.fwupd.enable = true;

  # Networking
  networking.networkmanager.enable = true;

  # Base system packages
  environment.systemPackages = with pkgs; [
    git
    vim
    wget
    curl
    pciutils
    usbutils
    nix-search-cli
  ];

  # User configuration
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;
}
