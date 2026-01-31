;;; conventional-commit.el --- Conventional commit helper with emojis -*- lexical-binding: t; -*-

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

(provide 'conventional-commit)
;;; conventional-commit.el ends here
