{ pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs
  ];

  home.file.".emacs.d/init.el" = {
    source = ../emacs-config/init.el;
  };

  home.file.".emacs.d/vitesse-dark-theme.el" = {
    source = ../emacs-config/vitesse-dark-theme.el;
  };
}
