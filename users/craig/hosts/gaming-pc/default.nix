{ config, pkgs, modules, disko, ... }:

{
  imports = [
    ../../common.nix
    #../../vm-testing.nix
    ./hardware.nix
    (disko + "/gaming-pc.nix")

    # Desktop
    (modules + "/desktop/gnome.nix")
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

  # Kernel - CachyOS with sched_ext (uncomment for AMD gaming PC)
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  boot.kernelPackages = pkgs.linuxPackages_zen;
  services.fwupd.enable = true;
  networking.networkmanager.enable = true;
  programs.fish.enable = true;
  security.sudo.wheelNeedsPassword = false;

  environment.systemPackages = with pkgs; [
    git vim wget curl pciutils usbutils nix-search-cli
  ];

  system.stateVersion = "25.11";
}
