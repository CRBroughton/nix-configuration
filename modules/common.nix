# Packages and settings shared across all systems
{ pkgs, ... }:

{
  imports = [ ./nix.nix ];

  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    nix-search-cli
    just
    restic
    micro
    vim
    wget
    curl
    direnv
    nvd
    nix-output-monitor
    nh
  ];
}
