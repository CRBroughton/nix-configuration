# Generic git configuration - enables git and common tools
{ config, pkgs, ... }:

{
  programs.git = {
    enable = true;
    settings = {
      init.defaultBranch = "master";
    };
  };

  home.packages = with pkgs; [
    lazygit
  ];
}
