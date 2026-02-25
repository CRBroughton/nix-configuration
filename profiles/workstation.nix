# Workstation profile - combines desktop, gaming, and development
# This is the full setup for laptop and gaming-pc
{ config, pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./gaming.nix
    ./development.nix
    ../modules/nixos/services/ssh.nix
    ../modules/nixos/services/tailscale.nix
    ../modules/nixos/services/vpn.nix
    ../modules/nixos/security.nix
    ../modules/nixos/nix.nix
  ];

  # Base system packages
  environment.systemPackages = with pkgs; [
    git
    vim
    wget
    curl
    pciutils
    usbutils
  ];

  # User configuration
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;
}
