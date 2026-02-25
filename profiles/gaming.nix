# Gaming profile - Steam, Gamemode, and graphics support
{ config, pkgs, ... }:

{
  imports = [
    ../modules/nixos/gaming.nix
  ];
}
