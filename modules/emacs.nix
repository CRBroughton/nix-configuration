{ pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs
  ];

  home.file.".emacs.d" = {
    source = ../emacs-config;
    recursive = true;
  };
}
