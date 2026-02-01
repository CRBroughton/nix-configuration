;;; packages.el --- Package configuration -*- lexical-binding: t; -*-

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package
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
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; projectile: Project management - finds files across entire project
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy))

;; counsel-projectile: Ivy integration for projectile
(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode +1)
  :bind (("C-p" . counsel-projectile-find-file)))  ; Fuzzy find file in project

;; ivy-rich: Adds extra information to ivy buffers (file sizes, docstrings, etc)
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; doom-modeline: A fancy, minimal modeline from the Doom Emacs distribution.
;; Shows file info, git branch, errors, encoding, etc in a clean format.
;; Requires nerd fonts for icons (you're using FiraCode Nerd Font).
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

;; rainbow-delimiters: Colors matching parentheses/brackets with rainbow colors
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key: Shows available keybindings in a popup as you type
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0))

;; olivetti: Focus mode - centers text with fixed width
(use-package olivetti
  :config
  (setq olivetti-body-width 200)  ; Character width for centered content
  :bind (("C-M-z" . olivetti-mode)))  ; Toggle focus mode

;; Built-in tab-line-mode: per-window buffer tabs
(global-tab-line-mode 1)
(setq tab-line-new-button-show nil    ; Hide new tab button
      tab-line-close-button-show nil) ; Hide close button on tabs
;; Ctrl+PageUp/Down: switch tabs
(global-set-key (kbd "C-<prior>") 'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-<next>") 'tab-line-switch-to-next-tab)
;; Alt+PageUp/Down: switch windows/splits
(global-set-key (kbd "M-<prior>") 'other-window)
(global-set-key (kbd "M-<next>") (lambda () (interactive) (other-window -1)))

;;; --- AUTOCOMPLETION ---

;; Company: Modular text completion framework
;; This handles the popup menu for code completion (Intellisense)
(use-package company
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

;;; --- VUE & WEB DEVELOPMENT ---

;; Web-mode: Major mode for editing web templates (HTML, Vue, JSX)
(use-package web-mode
  :mode ("\\.vue\\'" . vue-mode) ; Associate .vue files with our custom vue-mode
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  
  ;; Define a specific mode for Vue so we can hook LSP only to Vue, not all HTML
  (define-derived-mode vue-mode web-mode "Vue"
    "A major mode derived from web-mode, for Vue.js editing."))

;; Eglot: The Emacs LSP Client
(use-package eglot
  :ensure t
  :hook (vue-mode . eglot-ensure) ;; Automatically start LSP in vue-mode
  :config
  ;; 1. Helper function to find the global TypeScript path (TSDK)
  ;; This is critical for Volar (Vue LS) to handle types correctly.
  (defun my/vue-eglot-init-options ()
    (let ((tsdk-path (expand-file-name
                      "lib"
                      (shell-command-to-string "npm list --global --parseable typescript | head -n1 | tr -d \"\n\""))))
      `(:typescript (:tsdk ,tsdk-path
                     :languageFeatures (:completion
                                        (:defaultTagNameCase "both"
                                         :defaultAttrNameCase "kebabCase"
                                         :getDocumentNameCasesRequest nil
                                         :getDocumentSelectionRequest nil)
                                        :diagnostics
                                        (:getDocumentVersionRequest nil))
                     :documentFeatures (:documentFormatting
                                        (:defaultPrintWidth 100
                                         :getDocumentPrintWidthRequest nil)
                                        :documentSymbol t
                                        :documentColor t)))))

  ;; 2. Register the Vue Language Server with the correct options
  (add-to-list 'eglot-server-programs
               `(vue-mode . ("vue-language-server" "--stdio" 
                             :initializationOptions ,(my/vue-eglot-init-options)))))

(provide 'packages)
;;; packages.el ends here
