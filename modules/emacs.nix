{ pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs
  ];

  home.file.".emacs.d/init.el".source = ../emacs-config/init.el;
  home.file.".emacs.d/ui.el".source = ../emacs-config/ui.el;
  home.file.".emacs.d/keybindings.el".source = ../emacs-config/keybindings.el;
  home.file.".emacs.d/packages.el".source = ../emacs-config/packages.el;
  home.file.".emacs.d/vitesse-dark-theme.el".source = ../emacs-config/vitesse-dark-theme.el;
}
