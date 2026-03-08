{ config, pkgs, modules, disko, ... }:

{
  imports = [
    ../../common.nix
    #../../vm-testing.nix
    ./hardware.nix
    (disko + "/laptop.nix")

    # Desktop
    (modules + "/desktop/gnome.nix")
    (modules + "/services/printing.nix")
    (modules + "/services/flatpak/base.nix")
    (modules + "/services/ssh.nix")
    (modules + "/tailscale.nix")
    (modules + "/services/vpn.nix")
    (modules + "/security.nix")
    (modules + "/nix.nix")
    (modules + "/gaming.nix")
    (modules + "/development.nix")

    # Personal
    ../../flatpaks.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    git vim wget curl pciutils usbutils nix-search-cli
  ];

  # Laptop power management
  services.power-profiles-daemon.enable = true;

  system.stateVersion = "25.11";
}
