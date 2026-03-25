# Host configuration for pc
{ pkgs, ... }:

{
  imports = [
    ./hardware.nix
    ./vm-testing.nix # Remove this line when deploying to real hardware
  ];

  # Boot loader
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Network
  networking.networkmanager.enable = true;

  security.sudo.wheelNeedsPassword = true;

  # Basic packages
  environment.systemPackages = with pkgs; [
    git
    vim
    curl
    wget
  ];

  # Enable modules (defined in modules/)
  # desktops.gnome.enable = true;
  # shell.enable = true;
}
