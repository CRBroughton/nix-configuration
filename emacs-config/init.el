(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)


(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 150)
(load-theme 'wombat)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Quick window switching
(global-set-key (kbd "M-o") 'other-window)

;; Open shell in a bottom split (like VS Code)
;; If eshell is already visible, open a new eshell buffer (tabbed style)
(defun open-shell-below ()
  "Open eshell in a split at the bottom, or new eshell buffer if one exists."
  (interactive)
  (let ((eshell-window (get-buffer-window "*eshell*")))
    (if eshell-window
        ;; Eshell visible: switch to it and create a new eshell buffer
        (progn
          (select-window eshell-window)
          (eshell t))  ; t argument creates a new eshell buffer
      ;; No eshell visible: split and open eshell
      (progn
        (split-window-below -15)
        (other-window 1)
        (eshell)))))
(global-set-key (kbd "C-t") 'open-shell-below)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; command-log-mode: Displays a buffer showing all commands you execute in
;; real-time. Useful for learning keybindings and creating tutorials/demos.
;; Enable with M-x command-log-mode, view with M-x clm/open-command-log-buffer
(use-package command-log-mode)

;; ivy: A generic completion framework that replaces Emacs' default completion.
;; Provides a minimal, fast interface for narrowing and selecting from lists
;; (buffers, files, commands, etc). Includes swiper for in-buffer search.
(use-package ivy
  :diminish
  :bind (("C-f" . swiper)  ; Find in buffer
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; counsel: Provides ivy-enhanced versions of common Emacs commands.
;; Replaces M-x, find-file, buffer switching, etc with ivy-powered versions
;; that offer fuzzy matching and better candidate display.
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-p" . counsel-find-file)  ; Open file (like VS Code)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))


;; doom-modeline: A fancy, minimal modeline from the Doom Emacs distribution.
;; Shows file info, git branch, errors, encoding, etc in a clean format.
;; Requires nerd fonts for icons (you're using FiraCode Nerd Font).
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
