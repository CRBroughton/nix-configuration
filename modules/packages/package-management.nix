{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # Package management
    flatpak
    nix-search-cli
    nixfmt
  ];
}
