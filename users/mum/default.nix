{ pkgs, ... }:

{
  imports = [ ./gnome.nix ];

  home.homeDirectory = "/home/mum";

  home.packages = with pkgs; [
    obs-studio
  ];
}
