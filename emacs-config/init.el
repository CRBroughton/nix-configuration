;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; Add config directory to load path
(add-to-list 'load-path "~/.emacs.d/")

;; Load configuration modules
(require 'ui)          ; UI and appearance settings
(require 'keybindings) ; Custom keybindings and functions
(require 'packages)    ; Package configuration

;;; init.el ends here
