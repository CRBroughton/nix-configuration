;;; git.el --- Git configuration (magit) -*- lexical-binding: t; -*-

;; magit: Git interface for Emacs
(use-package magit
  :bind (("C-x g" . magit-status))
  :config
  (define-key magit-status-mode-map (kbd "p") 'magit-pull-from-upstream)
  (define-key magit-status-mode-map (kbd "P") 'magit-push-current-to-upstream)
  (define-key magit-status-mode-map (kbd "l") 'magit-log-current)
  (define-key magit-status-mode-map (kbd "S") 'magit-branch-checkout)
  (define-key magit-status-mode-map (kbd "d") 'magit-diff-unstaged))

(provide 'git)
;;; git.el ends here
