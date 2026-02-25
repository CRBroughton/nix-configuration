{ config, pkgs, ... }:

{
  programs.neovim = {
    enable = true;
    defaultEditor = false;
  };

  # External neovim config
  xdg.configFile."nvim".source = ../../../config/neovim;

  home.packages = with pkgs; [
    tree-sitter
  ];
}
