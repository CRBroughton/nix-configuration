# Server profile - headless system with SSH and basic services
{ config, pkgs, ... }:

{
  imports = [
    ../modules/nixos/services/ssh.nix
    ../modules/nixos/services/tailscale.nix
    ../modules/nixos/nix.nix
  ];

  # Headless - no desktop
  # Just networking and SSH access
  networking.networkmanager.enable = true;
}
