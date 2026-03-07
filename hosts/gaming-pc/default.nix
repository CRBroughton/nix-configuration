{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    ./hardware.nix
    ../../disko/gaming-pc.nix
    ../../profiles/workstation.nix
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
