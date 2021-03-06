;;; init-color-theme-solarized.el --- init, theme

;; Copyright (C) 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, theme, solarized
;; creation time: [Tue Apr 12 01:35:26 2011]
;;; Commentary:

  ;;; Author: Ethan Schoonover, Solarized; Greg Pfeil, Emacs adaptation
  ;;; URL: http://ethanschoonover.com/solarized
  ;;; This file is not (YET) part of GNU Emacs.

(my-require 'color-theme)

;; 
(defvar default-font-color "#344050")
;; (defvar default-font-color "#657080")
(defvar default-background-color "#fdf6e3")
;; (defvar default-font-color "#657b83")
(defvar region-color "#e0e0b0")
;; (defvar region-color "#e0e0d0")
(defvar dummy-region-color "#eee0d0")
(defvar comment-face-color "#af8050")
(defvar highlight-font-color "orange2")
(defvar highlight-font-color-2 "orange4")
(defvar highlight-background-color "#eee8d5")

(defun color-theme-solarized (mode)
  "Color theme by Ethan Schoonover, created 2011-03-24.
Ported to Emacs by Greg Pfeil, http://ethanschoonover.com/solarized."
  (interactive "Slight or dark? ")
  (let ((base03  "#002b36")
        (base02  "#073642")
        (base01  "#586e75")
				(base00   default-font-color)
				;; (base00  "#657b83")
				(base0   default-font-color)
        ;; (base0   "#839496")
        (base1   "#93a1a1")
        ;; (base2   "#eee8d5")
				(base2   highlight-background-color)
        (base3   default-background-color)
				;; (base3   "#fdf6e3")
        (yellow  "#b58900")
        (orange  "#cb4b16")
        (red     "#dc322f")
        (magenta "#d33682")
        (violet  "#6c71c4")
        (blue    "#268bd2")
        (cyan    "#2aa198")
        (pink2   "pink2")
        (green   "#859900"))
    (when (eq 'light mode)
      (rotatef base03 base3)
      (rotatef base02 base2)
      (rotatef base01 base1)
      (rotatef base00 base0))
    (color-theme-install
     `(color-theme-solarized
       ((foreground-color . ,base0)
        (background-color . ,base03)
        (background-mode . ,mode)
        ;; (cursor-color . ,base0))
			  (cursor-color . "pink2"))
       ;; basic
       (default ((t (:foreground ,base0))))
       (cursor ((t (:foreground ,base0 :background ,base03 :inverse-video t))))
       (escape-glyph-face ((t (:foreground ,red))))
       (fringe ((t (:foreground ,base01 :background ,base02))))
       (header-line ((t (:foreground ,base0 :background ,pink2))))
       (highlight ((t (:background ,base02))))
       (isearch ((t (:foreground ,yellow :inverse-video t))))
       (menu ((t (:foreground ,base0 :background ,base02))))
       (minibuffer-prompt ((t (:foreground ,blue))))
       (mode-line
        ((t (:foreground ,base1 :background ,base02
                         :box (:line-width 1 :color ,base1)))))
       (mode-line-buffer-id ((t (:foreground ,base1))))
       (mode-line-inactive
        ((t (:foreground ,base0  :background ,base02
                         :box (:line-width 1 :color ,base02)))))
       ;; (region ((t (:background ,base01))))
			 (region ((t (:background ,region-color))))
       (secondary-selection ((t (:background ,base02))))
       (trailing-whitespace ((t (:foreground ,red :inverse-video t))))
       (vertical-border ((t (:foreground ,base0))))
       ;; compilation
       (compilation-info ((t (:forground ,green :bold t))))
       (compilation-warning ((t (:foreground ,orange :bold t))))
       ;; customize
       (custom-button
        ((t (:background ,base02 :box (:line-width 2 :style released-button)))))
       (custom-button-mouse ((t (:inherit custom-button :foreground ,base1))))
       (custom-button-pressed
        ((t (:inherit custom-button-mouse
                      :box (:line-width 2 :style pressed-button)))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-comment-tag ((t (:background ,base02))))
       (custom-documentation ((t (:inherit default))))
       (custom-group-tag ((t (:foreground ,orange :bold t))))
       (custom-link ((t (:foreground ,violet))))
       (custom-state ((t (:foreground ,green))))
       (custom-variable-tag ((t (:foreground ,orange :bold t))))
       ;; diff
       (diff-added ((t (:foreground ,green :inverse-video t))))
       (diff-changed ((t (:foreground ,yellow :inverse-video t))))
       (diff-removed ((t (:foreground ,red :inverse-video t))))
       ;; emacs-wiki
       (emacs-wiki-bad-link-face ((t (:foreground ,red :underline t))))
       (emacs-wiki-link-face ((t (:foreground ,blue :underline t))))
       (emacs-wiki-verbatim-face ((t (:foreground ,base00 :underline t))))
       ;; font-lock
       (font-lock-builtin-face ((t (:foreground ,green))))
			 (font-lock-comment-face ((t (:foreground ,comment-face-color))))
			 ;; (font-lock-comment-face ((t (:foreground ,comment-face-color :italic t))))
       ;; (font-lock-comment-face ((t (:foreground ,base01 :italic t))))
       (font-lock-constant-face ((t (:foreground ,cyan))))
       (font-lock-function-name-face ((t (:foreground ,blue))))
       (font-lock-keyword-face ((t (:foreground ,green))))
       (font-lock-string-face ((t (:foreground ,cyan))))
       (font-lock-type-face ((t (:foreground ,yellow))))
       (font-lock-variable-name-face ((t (:foreground ,blue))))
       (font-lock-warning-face ((t (:foreground ,red :bold t))))
       ;; info
       (info-xref ((t (:foreground ,blue :underline t))))
       (info-xref-visited ((t (:inherit info-xref :foreground ,magenta))))
       ;; org
       (org-hide ((t (:foreground ,base03))))
       (org-todo ((t (:foreground ,red :bold t))))
       (org-done ((t (:foreground ,green :bold t))))
       (org-clock-overlay ((t (:foreground ,highlight-font-color-2  :background ,dummy-region-color))))
       ;; show-paren
			 (show-paren-match-face ((t (:background ,green))))
       ;; (show-paren-match-face ((t (:background ,dummy-region-color :foreground ,base3))))
       (show-paren-mismatch-face ((t (:background ,red))))
			 ;; (show-paren-mismatch-face ((t (:background ,red :foreground ,base3))))
			 ))))

(defun color-theme-solarized-dark ()
  (interactive)
  (color-theme-solarized 'dark))

(defun color-theme-solarized-light ()
  (interactive)
  (color-theme-solarized 'light))

;; test...
(color-theme-solarized-light)

(provide 'init-color-theme-solarized)
