;;; init.el --- Main Emacs configuration entry point -*- lexical-binding: t; -*-

;; Add lisp directory to load path
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Load configuration modules
(require 'ui)                  ; UI and appearance settings
(require 'keybindings)         ; Custom keybindings and functions
(require 'packages)            ; Package configuration
(require 'git)                 ; Git/magit configuration
(require 'conventional-commit) ; Conventional commit helper

;;; init.el ends here
