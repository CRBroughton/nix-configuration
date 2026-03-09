# Packages and settings shared across all systems
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nixfmt
    nix-search-cli
  ];
}