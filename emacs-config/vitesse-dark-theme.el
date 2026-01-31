(deftheme vitesse-dark
  "A port of the Vitesse Dark VS Code theme by Anthony Fu.")

(let ((class '((class color) (min-colors 89)))
      ;; Vitesse Dark palette
      (bg           "#121212")
      (bg-soft      "#1e1e1e")
      (bg-highlight "#282828")
      (fg           "#dbd7ca")
      (fg-dim       "#959da5")
      (comment      "#758575")
      (string       "#c98a7d")
      (keyword      "#4d9375")
      (function     "#80a665")
      (variable     "#bd976a")
      (type         "#5da9a7")
      (number       "#4c9a91")
      (operator     "#cb7676")
      (constant     "#c99076")
      (builtin      "#cb7676")
      (warning      "#e9c46a")
      (error        "#cb7676")
      (selection    "#3c3c3c")
      (line-num     "#555555")
      (cursor       "#a0a0a0"))

  (custom-theme-set-faces
   'vitesse-dark

   ;; Basic faces
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,cursor))))
   `(region ((,class (:background ,selection))))
   `(highlight ((,class (:background ,bg-highlight))))
   `(hl-line ((,class (:background ,bg-soft))))
   `(fringe ((,class (:background ,bg))))
   `(vertical-border ((,class (:foreground ,bg-highlight))))
   `(minibuffer-prompt ((,class (:foreground ,keyword :bold t))))

   ;; Font lock faces (syntax highlighting)
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment :italic t))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-doc-face ((,class (:foreground ,comment :italic t))))
   `(font-lock-function-name-face ((,class (:foreground ,function))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,operator))))
   `(font-lock-preprocessor-face ((,class (:foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,variable))))
   `(font-lock-warning-face ((,class (:foreground ,warning :bold t))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,line-num :background ,bg))))
   `(line-number-current-line ((,class (:foreground ,fg :background ,bg-soft :bold t))))

   ;; Mode line
   `(mode-line ((,class (:background ,bg-highlight :foreground ,fg))))
   `(mode-line-inactive ((,class (:background ,bg-soft :foreground ,fg-dim))))
   `(mode-line-buffer-id ((,class (:foreground ,keyword :bold t))))

   ;; Search
   `(isearch ((,class (:background ,keyword :foreground ,bg :bold t))))
   `(lazy-highlight ((,class (:background ,bg-highlight :foreground ,fg))))

   ;; Ivy/Swiper
   `(ivy-current-match ((,class (:background ,bg-highlight :extend t))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,keyword))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,function :bold t))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,string :bold t))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,variable :bold t))))
   `(swiper-match-face-1 ((,class (:foreground ,keyword))))
   `(swiper-match-face-2 ((,class (:foreground ,function :bold t))))
   `(swiper-match-face-3 ((,class (:foreground ,string :bold t))))
   `(swiper-match-face-4 ((,class (:foreground ,variable :bold t))))
   `(swiper-line-face ((,class (:background ,bg-highlight :extend t))))

   ;; Doom modeline
   `(doom-modeline-bar ((,class (:background ,keyword))))
   `(doom-modeline-buffer-file ((,class (:foreground ,fg :bold t))))
   `(doom-modeline-buffer-modified ((,class (:foreground ,warning :bold t))))
   `(doom-modeline-project-dir ((,class (:foreground ,keyword))))

   ;; Parentheses
   `(show-paren-match ((,class (:background ,bg-highlight :foreground ,keyword :bold t))))
   `(show-paren-mismatch ((,class (:background ,error :foreground ,bg :bold t))))

   ;; Error/Warning
   `(error ((,class (:foreground ,error :bold t))))
   `(warning ((,class (:foreground ,warning :bold t))))
   `(success ((,class (:foreground ,keyword :bold t))))

   ;; Eshell
   `(eshell-prompt ((,class (:foreground ,keyword :bold t))))
   `(eshell-ls-directory ((,class (:foreground ,function :bold t))))
   `(eshell-ls-executable ((,class (:foreground ,keyword))))
   `(eshell-ls-symlink ((,class (:foreground ,type))))

   ;; Tab line (per-window tabs)
   `(tab-line ((,class (:background ,bg :foreground ,fg-dim))))
   `(tab-line-tab ((,class (:background ,bg :foreground ,fg-dim :box nil))))
   `(tab-line-tab-current ((,class (:background ,bg-soft :foreground ,fg :bold t :box nil))))
   `(tab-line-tab-inactive ((,class (:background ,bg :foreground ,fg-dim :box nil))))
   `(tab-line-highlight ((,class (:background ,bg-highlight))))

   ;; Header line
   `(header-line ((,class (:background ,bg :foreground ,fg-dim))))
   ))

(provide-theme 'vitesse-dark)
