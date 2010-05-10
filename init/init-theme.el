;;; init-theme.el --- theme setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, theme
;; creation time: Sun May  9 22:23:56 2010
;;; Commentary:

;; this setting base is pink-bllis.

;;; Code:

(eval-when-compile (require 'cl))

(when use-gui-setting

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
To check out the list, evaluate
\(list-colors-display my-theme-foreground-colors).")

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
	 '((default ((t ( :background "gray4" :foreground "gray78"))))
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
	   (minibuffer-prompt ((t (:foreground "pink"))))
	   (tool-bar ((t (:background "pink"
				      :box (:line-width 1 :style released-button)))))
	   (tooltip ((t (:background "lemon chiffon" :foreground "violet red"))))

	   ;; font-lock
	   (font-lock-builtin-face ((t (:foreground "SkyBlue"))))
	   (font-lock-comment-delimiter-face ((t (:foreground "green"))))
	   (font-lock-comment-face ((t (:foreground "green"))))
	   (font-lock-constant-face ((t (:foreground "Pink"))))
	   (font-lock-string-face ((t (:foreground "sandy brown"))))
	   (font-lock-doc-face ((t (:foreground "coral"))))
	   (font-lock-function-name-face ((t (:foreground "pink"))))
	   (font-lock-variable-name-face ((t (:foreground "PaleTurquoise"))))
	   (font-lock-keyword-face ((t (:foreground "LightBlue"))))
	   (font-lock-negation-char-face ((t (:foreground "red"))))
	   (font-lock-preprocessor-face ((t (:foreground "pink"))))
	   (font-lock-type-face ((t (:foreground "pink"))))
	   (font-lock-warning-face ((t (:bold t :foreground "red"))))
	   
	   ;; isearch
	   (isearch ((t (:foreground "white" :background "dark green"))))
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
   ((my-is-windows)	(setq my-font-size-base 120))
   ((my-is-mac)		(setq my-font-size-base 150))
   )

  (when use-font-setting
    (set-face-attribute 'default nil
			:family "VL Pゴシック"
			;; :family "VL Pゴシック"
                        :height my-font-size-base)

    ;; (set-face-attribute 'default nil
    ;; 			:family "VL ゴシック"
    ;; 			:height my-font-size-base)

    ;; (setq face-font-rescale-alist
    ;; 	  '((".*profont-medium.*" . (* my-font-size-base 1.5))
    ;; 	    (".*profont-bold.*" . (* my-font-size-base 1.8))
    ;; 	    ("-cdac$" . (* my-font-size-base 1.8))))
    )

  ;;-------------------------------
  ;; frame setting
  ;;-------------------------------
  ;; default frame setting
  (defvar my-frame-setting-list
    (list
     ;; '(background-color . "gray4") ;; 背景色
     ;; '(foreground-color . "gray75") ;; 文字色
     ;; '(cursor-color . "Green") ;; カーソル色
     ;; '(cursor-type . box) ;; カーソル形状
     ;; '(cursor-height . 4) ;; カーソルの高さ
     ;; '(mouse-color . "white") ;; マウスカーソル色
     ;; '(border-color . "white") ;; 縁の色
     ;; '(vertical-scroll-bars . 'nil) ;; スクロールバー
     '(width . 160) ;; 横幅(桁数)
     '(height . 82)	;; 高さ(行数)
     '(left . 0) ;; 左上隅 x 座標
     '(top . 0) ;; 左上隅 y 座標
     '(alpha . 100) ;; 透明度設定
     )
    )

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
  (global-font-lock-mode 1)			  ;;色を付ける
  (setq font-lock-maximum-decoration t)
  
  ;;タイトルバーにファイル名を表示するタイトルフォーマット設定
  (setq frame-title-format
        (format "emacs %s : %%f %s"
                (system-name)
                (emacs-version)))

  (display-time) ;;モードラインに時間を表示する
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
  (require 'hl-line)
  ;; (customize-set-value 'hl-line-face 'underline)
  (customize-set-value 'hl-line-face 'highlight)
  (global-hl-line-mode t)
  
  ;;-------------------------------
  ;; 対応する括弧を強調表示
  ;;-------------------------------
  (require 'paren)							;;対応する括弧を強調表示 & 対応しない場合は 警告する。
  (show-paren-mode t)							;;対応する括弧を表示する
  (set-face-background 'show-paren-match "gray50")
  (custom-set-variables
   '(show-paren-ring-bell-on-mismatch t) ;;対応しない括弧を探す。
   '(show-paren-style 'mixed) ;;対応する括弧が画面内になければ、括弧内を強調表示。
   )
  ;; より広範囲に色づけ
  (require 'highlight-parentheses)
  (highlight-parentheses-mode)
  (custom-set-variables
   '(hl-paren-colors '("orange" "blue" "yellow" "red"))
   '(hl-paren-background-colors nil)
   '(hl-paren-background-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
   )

  ;;-------------------------------
  ;; 折り返し設定
  ;;-------------------------------
  ;; (setq truncate-lines t)
  ;; (setq truncate-partial-width-windows t) ;; 縦分割画面で、長い行をwrapさせる。aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  (add-hook 'find-file-hook
	    '(lambda ()
	       (setq truncate-lines t)
	       (setq truncate-partial-width-windows t)
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
  ;; mac だと通常のfont-lockと競合してしまう
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
  ;; mac だとやけに遅い。
  ;;-------------------------------
  (when (my-is-windows)
    (require 'cursor-chg)
    (custom-set-variables
     '(curchg-change-cursor-on-overwrite/read-only-flag t)
     '(curchg-change-cursor-on-input-method-flag t)
     '(cursor-mode t) ; On for overwrite/read-only/input mode
     '(curchg-idle-cursor-type 'hbar)
     '(curchg-overwrite/read-only-cursor-type 'hollow)
     )

    (toggle-cursor-type-when-idle t) ; On when idle
    ;; (setq curchg-idle-cursor-type 'hbar)
    )

  (my-theme-set)

  
  (defun my-text-properties-at-point ()
    ""
    (interactive)
    (let ((prop (text-properties-at (point))))
      (message prop)))
  (global-set-key (kbd "C-l C-@ C-p") 'my-text-properties-at-point)

  )

(provide 'init-theme)