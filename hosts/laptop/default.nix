{ config, pkgs, ... }:

{
  imports = [
    ../common.nix
    ./hardware.nix
    ../../disko/laptop.nix
    ../../profiles/workstation.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_zen;

  system.stateVersion = "25.11";
}
