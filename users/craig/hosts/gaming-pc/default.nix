{ config, pkgs, ... }:

{
  imports = [
    ../../common.nix
    #../../vm-testing.nix  # Remove this line for production
    ./hardware.nix
    ../../../../disko/gaming-pc.nix
    # Compose profiles directly
    ../../../../profiles/graphical.nix
    ../../../../profiles/gaming.nix
    ../../../../profiles/development.nix
    # Personal flatpaks
    ../../flatpaks.nix
  ];

  # Kernel - CachyOS with sched_ext (uncomment for AMD gaming PC)
  # boot.kernelPackages = pkgs.linuxPackages_cachyos;
  # services.scx = {
  #   enable = true;
  #   scheduler = "scx_lavd";
  # };

  boot.kernelPackages = pkgs.linuxPackages_zen;

  system.stateVersion = "25.11";
}
