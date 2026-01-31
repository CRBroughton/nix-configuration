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

;; magit: Git interface for Emacs
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (define-key magit-status-mode-map (kbd "p") 'magit-pull)
  (define-key magit-status-mode-map (kbd "P") 'magit-push)
  (define-key magit-status-mode-map (kbd "l") 'magit-log-current)
  (define-key magit-status-mode-map (kbd "S") 'magit-branch-checkout)
  (define-key magit-status-mode-map (kbd "d") 'magit-diff-unstaged))

;; Conventional commit helper with emojis
(defvar conventional-commit-types
  '(("none"     ""   . "Plain commit (no type/emoji)")
    ("feat"     "✨" . "A new feature")
    ("fix"      "🐛" . "A bug fix")
    ("docs"     "📚" . "Documentation only changes")
    ("style"    "💎" . "Code style (formatting, semicolons, etc)")
    ("refactor" "♻️"  . "Code change that neither fixes a bug nor adds a feature")
    ("perf"     "⚡" . "Performance improvement")
    ("test"     "🧪" . "Adding or fixing tests")
    ("build"    "📦" . "Build system or dependencies")
    ("ci"       "🔧" . "CI configuration")
    ("chore"    "🔨" . "Other changes (no src or test)")
    ("revert"   "⏪" . "Reverts a previous commit"))
  "Conventional commit types with emojis and descriptions.")

(defun conventional-commit ()
  "Create a conventional commit message interactively with emoji."
  (interactive)
  (let* ((type (ivy-read "Commit type: "
                         (mapcar (lambda (x)
                                   (if (string= (car x) "none")
                                       (format "   %-10s %s" (car x) (cddr x))
                                     (format "%s %-10s %s" (cadr x) (car x) (cddr x))))
                                 conventional-commit-types)
                         :require-match t))
         (type-entry (seq-find (lambda (x) (string-match-p (regexp-quote (car x)) type))
                               conventional-commit-types))
         (type-name (car type-entry))
         (emoji (cadr type-entry))
         (is-plain (string= type-name "none"))
         (scope (if is-plain "" (read-string "Scope (optional): ")))
         (description (read-string (if is-plain "Message: " "Description: ")))
         (breaking (if is-plain nil (y-or-n-p "Breaking change? ")))
         (commit-msg (if is-plain
                         description
                       (concat type-name
                               (if (string-empty-p scope) "" (format "(%s)" scope))
                               (if breaking "!" "")
                               ": "
                               (if (string-empty-p emoji) "" (concat emoji " "))
                               description))))
    (when (and (not (string-empty-p description))
               (magit-toplevel))
      (magit-commit-create (list "-m" commit-msg)))))

(global-set-key (kbd "C-x c") 'conventional-commit)

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

(provide 'packages)
;;; packages.el ends here
