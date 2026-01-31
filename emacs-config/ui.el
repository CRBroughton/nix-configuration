;;; ui.el --- UI and appearance settings -*- lexical-binding: t; -*-

;; Disable UI clutter
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

;; Font
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 150)

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/")
(load-theme 'vitesse-dark t)

;; Show relative line numbers everywhere
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Disable line numbers in terminal buffers
(dolist (mode '(eshell-mode-hook term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(provide 'ui)
;;; ui.el ends here
