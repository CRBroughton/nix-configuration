# Packages and settings shared across all systems
{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    nix-search-cli
    just
    restic
    micro
    vim
    wget
    curl
    direnv
  ];
}
