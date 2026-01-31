{ pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs
  ];

  home.file.".config/emacs" = {
    source = ../emacs-config;
    recursive = true;
  };
}
