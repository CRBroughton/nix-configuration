{ pkgs, ... }:

{
  home.packages = with pkgs; [
    emacs
  ];

  home.file.".emacs.d/init.el".source = ../emacs-config/init.el;
  home.file.".emacs.d/lisp/ui.el".source = ../emacs-config/ui.el;
  home.file.".emacs.d/lisp/keybindings.el".source = ../emacs-config/keybindings.el;
  home.file.".emacs.d/lisp/packages.el".source = ../emacs-config/packages.el;
  home.file.".emacs.d/lisp/git.el".source = ../emacs-config/git.el;
  home.file.".emacs.d/lisp/conventional-commit.el".source = ../emacs-config/conventional-commit.el;
  home.file.".emacs.d/vitesse-dark-theme.el".source = ../emacs-config/vitesse-dark-theme.el;
}
