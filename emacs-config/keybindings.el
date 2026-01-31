;;; keybindings.el --- Custom keybindings and functions -*- lexical-binding: t; -*-

;; ESC: quit prompts in minibuffer, close window+buffer for terminal, close buffer otherwise
(defun smart-escape ()
  "Close buffer/tab smartly based on context:
- Minibuffer: escape/quit
- Terminal: kill buffer and close window if not last
- Buffer in multiple windows: remove from this window's tabs only
- Only one tab left and multiple windows: close the window
- Buffer in single window: kill the buffer"
  (interactive)
  (cond
   ((minibufferp) (keyboard-escape-quit))
   ((derived-mode-p 'eshell-mode 'term-mode)
    (kill-buffer (current-buffer))
    (when (> (count-windows) 1)
      (delete-window)))
   ;; Buffer displayed in multiple windows - remove from this window only
   ((> (length (get-buffer-window-list (current-buffer) nil t)) 1)
    (let ((buf (current-buffer))
          (win (selected-window)))
      ;; Switch to previous buffer
      (switch-to-prev-buffer)
      ;; Remove from this window's buffer lists so it won't show in tabs
      (set-window-prev-buffers win
        (assq-delete-all buf (window-prev-buffers win)))
      (set-window-next-buffers win
        (delq buf (window-next-buffers win)))))
   ;; Only one tab in this window and multiple windows exist - close window
   ((and (> (count-windows) 1)
         (<= (length (funcall tab-line-tabs-function)) 1))
    (delete-window))
   ;; Buffer only in this window - kill it
   (t (kill-buffer (current-buffer)))))
(global-set-key (kbd "<escape>") 'smart-escape)

;; Save with C-s (like VS Code)
(global-set-key (kbd "C-s") 'save-buffer)

;; Standard copy/paste/undo (like VS Code)
;; cua-mode: C-c copies only when text is selected, otherwise works as prefix
(cua-mode t)
(global-set-key (kbd "C-z") 'undo)

;; Split window right and switch to it (like VS Code Ctrl+\)
(defun split-window-right-and-switch ()
  "Split window right and move cursor to new window."
  (interactive)
  (split-window-right)
  (other-window 1))
(global-set-key (kbd "C-\\") 'split-window-right-and-switch)

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

;; Keybinding cheatsheet
(defun show-keybindings ()
  "Show a buffer with custom keybindings."
  (interactive)
  (let ((buf (get-buffer-create "*Keybindings*")))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "╔══════════════════════════════════════════════════════════════╗\n")
      (insert "║                    CUSTOM KEYBINDINGS                        ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  GENERAL                                                     ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  C-s          Save buffer                                    ║\n")
      (insert "║  C-z          Undo                                           ║\n")
      (insert "║  C-c          Copy (when text selected)                      ║\n")
      (insert "║  C-v          Paste                                          ║\n")
      (insert "║  C-x          Cut (when text selected)                       ║\n")
      (insert "║  ESC          Close buffer/tab/window (smart)                ║\n")
      (insert "║  F5           Reload config                                  ║\n")
      (insert "║  C-?          Show this help                                 ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  FILES & SEARCH                                              ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  C-p          Find file in project (fuzzy)                   ║\n")
      (insert "║  C-f          Find in buffer (swiper)                        ║\n")
      (insert "║  M-x          Command palette                                ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  WINDOWS & TABS                                              ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  C-\\          Split window right                             ║\n")
      (insert "║  C-t          Open terminal below                            ║\n")
      (insert "║  C-PageUp     Previous tab                                   ║\n")
      (insert "║  C-PageDown   Next tab                                       ║\n")
      (insert "║  M-PageUp     Previous window                                ║\n")
      (insert "║  M-PageDown   Next window                                    ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  GIT                                                         ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  C-x g        Magit status                                   ║\n")
      (insert "║  C-x c        Conventional commit                            ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  CODE NAVIGATION (Elisp)                                     ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  M-.          Go to definition                               ║\n")
      (insert "║  M-,          Go back                                        ║\n")
      (insert "║  C-h f        Describe function                              ║\n")
      (insert "║  C-h v        Describe variable                              ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  MODES                                                       ║\n")
      (insert "╠══════════════════════════════════════════════════════════════╣\n")
      (insert "║  C-M-z        Toggle focus mode (olivetti)                   ║\n")
      (insert "╚══════════════════════════════════════════════════════════════╝\n")
      (goto-char (point-min))
      (read-only-mode 1)
      (local-set-key (kbd "q") 'kill-buffer-and-window)
      (local-set-key (kbd "<escape>") 'kill-buffer-and-window))
    (pop-to-buffer buf)))

(global-set-key (kbd "C-?") 'show-keybindings)

(provide 'keybindings)
;;; keybindings.el ends here
