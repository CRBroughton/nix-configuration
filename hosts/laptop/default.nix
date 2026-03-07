{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    #../vm-testing.nix  # Remove this line for production
    ./hardware.nix
    ../../disko/laptop.nix
    ../../profiles/workstation.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;

  # Laptop power management
  services.power-profiles-daemon.enable = true;

  system.stateVersion = "25.11";
}
