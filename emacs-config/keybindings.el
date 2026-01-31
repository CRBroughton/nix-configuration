;;; keybindings.el --- Custom keybindings and functions -*- lexical-binding: t; -*-

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Save with C-s (like VS Code)
(global-set-key (kbd "C-s") 'save-buffer)

;; Standard copy/paste/undo (like VS Code)
;; cua-mode: C-c copies only when text is selected, otherwise works as prefix
(cua-mode t)
(global-set-key (kbd "C-z") 'undo)

;; Quick window switching
(global-set-key (kbd "M-o") 'other-window)

;; Reload config (like VS Code reload window)
(defun reload-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))
(global-set-key (kbd "<f5>") 'reload-config)

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

(provide 'keybindings)
;;; keybindings.el ends here
