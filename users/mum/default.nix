{ pkgs, ... }:

{
  imports = [
    ./gnome.nix
    ./zen.nix
  ];

  home.homeDirectory = "/home/mum";

  home.packages = with pkgs; [
    obs-studio
  ];
}
