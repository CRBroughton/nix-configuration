# Development profile - containers, VMs, and dev tools
{ config, pkgs, ... }:

{
  imports = [
    ../modules/nixos/virtualisation.nix
  ];
}
