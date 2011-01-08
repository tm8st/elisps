;;; init-theme.el --- theme setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, theme
;; creation time: Sun May  9 22:23:56 2010
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(when use-gui-setting

  ;; (when (my-is-mac)
  ;;   (set-default-font "Inconsolata-11")
  ;;   (set-face-font 'variable-pitch "Inconsolata-11")
  ;;   (set-fontset-font (frame-parameter nil 'font)
  ;;                  'japanese-jisx0208
  ;;                  '("Takaoゴシック" . "unicode-bmp")))

  (defvar my-theme-foreground-colors
    (let ((candidates)
          ;; (red-limit #xe000)
          (green-limit #xed00)
          (both-limit #xa000))
      (dolist (item color-name-rgb-alist)
        (destructuring-bind (color red green blue) item
          (when (and (not (color-gray-p color))
                     ;; (< red red-limit)
                     (< green green-limit)
                     (not (and (> red both-limit)
                               (> green both-limit))))
            (setq candidates (cons color candidates)))))
      candidates)
    "Colors to use for nicks in rcirc, for example.
To check out the list, evaluate (list-colors-display my-theme-foreground-colors).")

  (defun my-theme-set ()
    "set my color setting."
    (interactive)

    ;; variables
    (customize-set-variable 'CUA-mode-read-only-cursor-color "dark grey")
    (customize-set-variable 'help-highlight-face 'info-xref)
    (customize-set-variable 'list-matching-lines-buffer-name-face 'bold)
    (customize-set-variable 'rcirc-colors 'my-theme-foreground-colors)

    ;; faces
    (dolist
        (element
         '((default ((t ( :background "#cfcca9" :foreground "gray75"))))
           (button ((t (:bold t))))
           (cursor ((t (:background "Orange"))))
           (highlight ((t (:background "Gray30"))))
           (region ((t (:background "DarkOrange4"))))
           (fringe ((t (:background "gray40"))))
           (menu ((t (:background "moccasin" :foreground "black"))))
           (modeline ((t (:background "gray25" :foreground "gray75"
                                      :box (:line-width 1 :style released-button)))))
           (mode-line-inactive ((t (:background "gray10" :foreground "gray60"
                                                :box (:line-width 1 :style released-button)))))
           (minibuffer-prompt ((t (:foreground "pink2"))))
           (tool-bar ((t (:background "pink2"
                                      :box (:line-width 1 :style released-button)))))
           (tooltip ((t (:background "lemon chiffon" :foreground "violet red"))))

           ;; font-lock
           (font-lock-builtin-face ((t (:foreground "turquoise3"))))
           (font-lock-comment-delimiter-face ((t (:foreground "GreenYellow"))))
           (font-lock-comment-face ((t (:foreground "GreenYellow"))))
           (font-lock-constant-face ((t (:foreground "Pink2"))))
           (font-lock-string-face ((t (:foreground "chocolate1"))))
           (font-lock-doc-face ((t (:foreground "coral"))))
           (font-lock-function-name-face ((t (:foreground "pink2"))))
           (font-lock-variable-name-face ((t (:foreground "PaleTurquoise"))))
           (font-lock-keyword-face ((t (:foreground "CadetBlue1"))))
           (font-lock-negation-char-face ((t (:foreground "red"))))
           (font-lock-preprocessor-face ((t (:foreground "pink2"))))
           (font-lock-type-face ((t (:foreground "pink2"))))
           (font-lock-warning-face ((t (:bold t :foreground "red"))))

           ;; isearch
           (isearch ((t (:foreground "white" :background "darkgreen"))))
           (isearch-lazy-highlight-face ((t (:foreground "bisque"))))

           ;; ;; info-mode
           ;; (header-line ((t (:background "deep pink" :foreground "pink"))))
           ;; ;; calendar
           ;; (calendar-today-face ((t (:foreground "lemon chiffon"))))
           ;; (diary-face ((t (:bold t :foreground "yellow"))))
           ;; (holiday-face ((t (:bold t :foreground "peru"))))
           ;; ;; cperl
           ;; (cperl-array-face ((t (:bold t :foreground "tomato"))))
           ;; (cperl-hash-face  ((t (:bold t :foreground "chocolate"))))
           ;; (cperl-nonoverridable-face  ((t (:foreground "red"))))
           ;; ;; makefiles
           ;; (makefile-shell-face  ((t (:background "linen"))))

           ))
      (let ((face (car element))
            (spec (nth 1 element)))
        (unless (facep face)
          (make-face face))
        (face-spec-set face spec))))

  ;;-------------------------------
  ;; Font setting
  ;;-------------------------------
  ;; ベースのフォントサイズ OSによって微妙に変更
  (defvar my-font-size-base 100)
  (cond
   ((my-is-windows) (setq my-font-size-base 135))
   ;; ((my-is-mac)      (setq my-font-size-base 300))
   ((my-is-mac) (setq my-font-size-base 180)))

  (when (and use-font-setting (my-is-windows)
             (set-face-attribute 'default nil
                                 :family "VL ゴシック"
                                 :height my-font-size-base)))

  (when (and use-font-setting (my-is-mac)
             (set-face-attribute 'default nil
                                 ;; :family "VL ゴシック"
                                 :height my-font-size-base)))

  ;; (set-face-attribute 'default nil
  ;;                    :family "VL ゴシック"
  ;;                    :height my-font-size-base)

  ;; (setq face-font-rescale-alist
  ;;      '((".*profont-medium.*" . (* my-font-size-base 1.5))
  ;;        (".*profont-bold.*" . (* my-font-size-base 1.8))
  ;;        ("-cdac$" . (* my-font-size-base 1.8))))


  ;;-------------------------------
  ;; frame setting
  ;;-------------------------------
  ;; default frame setting
  (defvar my-frame-setting-list
    (list
     '(background-color . "#005400") ;; 背景色
     '(foreground-color . "gray75") ;; 文字色
     '(cursor-color . "orange") ;; カーソル色
     '(cursor-type . bar) ;; カーソル形状
     '(cursor-height . 4) ;; カーソルの高さ
     '(mouse-color . "white") ;; マウスカーソル色
     '(border-color . "black") ;; 縁の色
     '(vertical-scroll-bars . 'nil) ;; スクロールバー
     '(width . 260) ;; 横幅(桁数)
     '(height . 160)    ;; 高さ(行数)
     '(left . 0) ;; 左上隅 x 座標
     '(top . 0) ;; 左上隅 y 座標
     '(alpha . 100) ;; 透明度設定
     ))

  ;; (add-to-list 'default-frame-alist my-frame-setting-list)
  ;; (add-to-list 'initial-frame-alist my-frame-setting-list)

  (setq default-frame-alist
        (append
         my-frame-setting-list
         default-frame-alist))

  (setq initial-frame-alist
        (append
         my-frame-setting-list
         initial-frame-alist))

  ;; font lock
  (setq font-lock-support-mode 'jit-lock-mode)
  (global-font-lock-mode 1)                       ;;色を付ける
  (setq font-lock-maximum-decoration t)

  ;;タイトルバーにファイル名を表示するタイトルフォーマット設定
  (setq frame-title-format
        (format "emacs %s : %%f %s"
                (system-name)
                (emacs-version)))

  (display-time) ;;モードラインに時間を表示する
  (setq display-time-day-and-date t)
  (which-function-mode 1)  ;;現在の関数名をモードラインに表示
  (column-number-mode t) ;;カーソルの位置が何文字目かを表示する
  (line-number-mode t) ;;カーソルの位置が何行目かを表示する

  ;;-------------------------------
  ;; 日本語入力のON/OFFでカーソルの色を変える
  ;;-------------------------------
  (when (my-is-windows)
    (add-hook 'mw32-ime-on-hook
              (function (lambda () (set-cursor-color "Pink"))))
    (add-hook 'mw32-ime-off-hook
              (function (lambda () (set-cursor-color "White"))))
    (setq-default mw32-ime-mode-line-state-indicator "[--]")
    )

  ;;-------------------------------
  ;; 現在行の強調
  ;;-------------------------------
  (when (require 'hl-line nil t)
    (defface my-hl-line-face
      '((t (:background "gray10")))
      "Face for displaying line numbers in the display margin."
      :group 'yalinum)

    ;; (customize-set-value 'hl-line-face 'underline)
    (customize-set-value 'hl-line-face 'my-hl-line-face)
    ;; (customize-set-value 'hl-line-face 'highlight)
    (global-hl-line-mode t)
    )

  ;;-------------------------------
  ;; 対応する括弧を強調表示
  ;;-------------------------------
  (require 'paren)                                                      ;;対応する括弧を強調表示 & 対応しない場合は 警告する。
  (show-paren-mode t)                                                   ;;対応する括弧を表示する
  (set-face-background 'show-paren-match "gray30")
  (custom-set-variables
   '(show-paren-ring-bell-on-mismatch t) ;;対応しない括弧を探す。
   '(show-paren-style 'mixed) ;;対応する括弧が画面内になければ、括弧内を強調表示。
   )
  ;; より広範囲に色づけ
  (require 'highlight-parentheses)
  (highlight-parentheses-mode)
  (custom-set-variables
   ;; '(hl-paren-colors '("orange" "blue" "yellow" "red"))
   '(hl-paren-background-colors nil)
   '(hl-paren-background-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
   )

  ;;-------------------------------
  ;; 折り返し設定
  ;;-------------------------------
  (add-hook 'find-file-hook
            '(lambda ()
               (unless (string-match "\\.txt$" (buffer-file-name))
                 (setq truncate-lines t)
                 (setq truncate-partial-width-windows t))
               )
            )

  (set-scroll-bar-mode nil) ;; スクロールバーを表示させない
  ;; (set-scroll-bar-mode 'right) ;; スクロールバーを右側に表示する
  (blink-cursor-mode 0) ;;カーソルの点滅を止める
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)

  ;;-------------------------------------
  ;; タブや全角スペースを表示
  ;; mac だと通常のfont-lockと競合してしまう???
  ;;-------------------------------------
  (when (my-is-windows)
    (defface my-face-full-space
      '((t :background "gray60"))
      "full space face"
      :group 'my)

    (defvar my-face-full-space 'my-face-full-space
      "full space face.")

    (defadvice font-lock-mode (before my-font-lock-mode ())
      (font-lock-add-keywords
       major-mode
       '(
         ;; ("\t" 0 my-face-tab append)
         ("　" 0 my-face-full-space append)
         )))
    (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
    (ad-activate 'font-lock-mode)
    )

  ;;-------------------------------
  ;; コンテキストに合わせてカーソル変更
  ;;-------------------------------
  (require 'cursor-chg)
  (custom-set-variables
   '(curchg-change-cursor-on-overwrite/read-only-flag t)
   '(curchg-change-cursor-on-input-method-flag t)
   '(cursor-mode t) ; On for overwrite/read-only/input mode
   '(curchg-idle-cursor-type 'hbar)
   '(curchg-default-cursor-type 'bar)
   '(curchg-overwrite/read-only-cursor-type 'hollow))

 (toggle-cursor-type-when-idle t) ; On when idle

  ;;-------------------------------
  ;; diff
  ;;-------------------------------
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(diff-added ((t (:foreground "pink"))))
   '(diff-context ((t nil)))
   '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
   '(diff-function ((t (:foreground "Orange"))))
   '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
   '(diff-hunk-header ((t (:foreground "turquoise3"))))
   '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
   '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
   '(diff-removed ((t (:foreground "red")))))

  ;;;-------------------------------
  ;;; yalinum
  ;;;-------------------------------
  (require 'yalinum)
  (global-yalinum-mode t)
  (global-linum-mode -1)

  (set-face-foreground 'yalinum-face "gray70")
  (set-face-foreground 'yalinum-bar-face "Pink")

  (set-face-background 'yalinum-face "#005400")
  (set-face-background 'yalinum-bar-face "#005400")

  (require 'jaunte)
  (set-face-foreground 'jaunte-hint-face "pink")
  (set-face-background 'jaunte-hint-face "black")

  (customize-set-variable 'yalinum-line-number-length-min 5)
  (customize-set-variable 'yalinum-eager nil)
    
  (when (my-is-mac)
    (customize-set-variable 'yalinum-width-base 1)
    (customize-set-variable 'yalinum-width-scale 0.85)
    (customize-set-variable 'yalinum-line-number-display-format "%0$numd ")
    )
  (when (my-is-windows)
    (customize-set-variable 'yalinum-width-base 0)
    (customize-set-variable 'yalinum-width-scale 1)
    (customize-set-variable 'yalinum-line-number-display-format " %0$numd ")
    )

  (setq frame-alpha-lower-limit 0.01)

  (defun what-face-at-point (pos)
    (interactive "d")
    (let ((face (or (get-char-property (point) 'read-face-name)
                    (get-char-property (point) 'face))))
      (if face (message "Face: %s" face) (message "No face at %d" pos))))

  (global-set-key (kbd "C-l C-l") 'what-face-at-point)

  (setq whitespace-style
	'(tabs tab-mark spaces space-mark))
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
	'((space-mark ?\x3000 [?\□])
	  (tab-mark   ?\t   [?\xBB ?\t])
	  ))

  ;; (require 'whitespace)
  ;; (global-whitespace-mode 1)
  ;; (set-face-foreground 'whitespace-space "LightSlateGray")
  ;; (set-face-background 'whitespace-space "DarkSlateGray")
  ;; (set-face-foreground 'whitespace-tab "LightSlateGray")
  ;; (set-face-background 'whitespace-tab "DarkSlateGray")

  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(highlight ((t (:background "black"))))
   '(howm-reminder-today-face ((t (:background "black" :foreground "Pink"))))
   '(howm-reminder-tomorrow-face ((t (:background "black" :foreground "gray70")))))
  (my-theme-set)
  )

(provide 'init-theme)
