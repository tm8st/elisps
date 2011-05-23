;;; init-theme.el --- theme setting

;; Copyright (C) 2010, 2011, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, theme

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

(when use-font-setting

  ;;-------------------------------
  ;; Font setting
  ;;-------------------------------
  (defvar my-font-size-base 100)
  (cond
   ((my-is-windows) (setq my-font-size-base 155))
   ((my-is-mac) (setq my-font-size-base 260)))

	(when (>= emacs-major-version 23)
		(when use-font-setting
			(when (my-is-windows)
				(set-face-attribute 'default nil
											:family "Ricty"
											:height my-font-size-base))
			(when (my-is-mac)
				(set-face-attribute 'default nil
														:family "Inconsolata"
														:height my-font-size-base)

				(set-fontset-font
				 (frame-parameter nil 'font)
				 'japanese-jisx0208
				 '("IPAGothic" . "iso10646-1"))

				(set-fontset-font
				 (frame-parameter nil 'font)
				 'japanese-jisx0212
				 '("IPAGothic" . "iso10646-1"))

				(set-fontset-font
				 (frame-parameter nil 'font)
				 'mule-unicode-0100-24ff
				 '("Inconsolata" . "iso10646-1"))
				)

			(setq face-font-rescale-alist
						'(("^-apple-hiragino.*" . 1.2)
							(".*osaka-bold.*" . 0.82)
							(".*osaka-medium.*" . 0.82)
							(".*VL\ .*" . 0.8)
							(".*IPAGothic.*" . 0.86)
							("NfMotoya Birch Std" . 0.8)
							(".*courier-bold-.*-mac-roman" . 1.0)
							(".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
							(".*monaco-bold-.*-mac-roman" . 0.9)
							("-cdac$" . 1.3)))))

  (require 'yalinum)
  (customize-set-variable 'yalinum-line-number-length-min 0)
  (customize-set-variable 'yalinum-eager nil)
	
  (when (my-is-mac)
    (customize-set-variable 'yalinum-width-base 0)
    (customize-set-variable 'yalinum-width-scale 1.0)
    (customize-set-variable 'yalinum-line-number-display-format " %0$numd ")
    )

  (when (my-is-windows)
    (customize-set-variable 'yalinum-width-base 0)
    (customize-set-variable 'yalinum-width-scale 1)
    (customize-set-variable 'yalinum-line-number-display-format " %0$numd ")
    ))

(when use-gui-setting

	(load "init-color-theme-solarized.el")

	(require 'anything)
	(set-face-foreground 'anything-header default-font-color)
	(set-face-background 'anything-header dummy-region-color)

	(require 'skk-vars)
	(skk-make-face 'default-font-color/dummy-region-color)
	(setq skk-henkan-face 'default-font-color/dummy-region-color)
	(customize-set-variable 'skk-use-face nil)

	(when skk-show-inline
		;; 変数 skk-treat-candidate-appearance-function を利用して自前で候補に
		;; 色を付ける場合はこの変数を nil に設定する。
		;; (setq skk-inline-show-face )
		(setq skk-inline-show-background-color dummy-region-color))

	;; (set-face-foreground 'skk-tooltip-face default-font-color)
	;; (set-face-background 'skk-tooltip-face dummy-region-color)

	;; 色設定
	(when (require 'bm nil t)
		(customize-set-value 'bm-priority 1)
		(set-face-foreground 'bm-face default-font-color)
		(set-face-background 'bm-face "#f0e0c0")
		(set-face-foreground 'bm-persistent-face default-font-color)
		(set-face-background 'bm-persistent-face "#f0e0c0")
		)

	(when (require 'tabbar nil t)
		(set-face-foreground 'tabbar-default-face default-font-color)
		(set-face-background 'tabbar-default-face dummy-region-color)
		(set-face-foreground 'tabbar-selected-face default-font-color)
		(set-face-background 'tabbar-selected-face region-color)
		)

	(when (require 'yalinum nil t)
		(set-face-foreground 'yalinum-face default-font-color)
		(set-face-foreground 'yalinum-bar-face default-font-color)
		(set-face-background 'yalinum-face default-background-color)
		(set-face-background 'yalinum-bar-face dummy-region-color)
		)

  
  (when (require 'popup nil t)
    (set-face-foreground 'ac-candidate-face default-font-color)
    (set-face-foreground 'ac-selection-face "pink3")
    (set-face-background 'ac-candidate-face dummy-region-color)
    (set-face-background 'ac-selection-face dummy-region-color)

    (set-face-foreground 'popup-menu-face default-font-color)
    (set-face-background 'popup-menu-face dummy-region-color)
    (set-face-foreground 'popup-menu-selection-face "pink3")
    (set-face-background 'popup-menu-selection-face dummy-region-color))

	(require 'yafastnav)
	(set-face-foreground 'yafastnav-shortcut-key-face-type "pink2")
	(set-face-background 'yafastnav-shortcut-key-face-type "#080304")
	;; (set-face-background 'yafastnav-shortcut-key-face-type dummy-region-color)

  (require 'jaunte)
  (set-face-foreground 'jaunte-hint-face highlight-font-color-2)
  (set-face-background 'jaunte-hint-face highlight-background-color)

	(setq my-popup-cadidate-color default-font-color)
	(setq my-popup-selection-color highlight-font-color)
	(setq my-popup-background-color highlight-background-color)

	(set-face-foreground 'ac-candidate-face my-popup-cadidate-color)
	(set-face-foreground 'ac-selection-face my-popup-selection-color)
	(set-face-background 'ac-candidate-face my-popup-background-color)
	(set-face-background 'ac-selection-face my-popup-background-color)

	(set-face-foreground 'popup-menu-face my-popup-cadidate-color)
	(set-face-foreground 'popup-menu-selection-face my-popup-selection-color)
	(set-face-background 'popup-menu-selection-face my-popup-background-color)
	(set-face-background 'popup-menu-selection-face my-popup-background-color)
	(set-face-background 'popup-menu-face my-popup-background-color)

	(set-face-foreground 'howm-view-hilit-face "black")
	(set-face-background 'howm-view-name-face dummy-region-color)
	(set-face-background 'howm-reminder-today-face dummy-region-color)
	(set-face-foreground 'howm-reminder-today-face highlight-font-color)
	(set-face-foreground 'howm-reminder-tomorrow-face highlight-font-color-2)
	(set-face-background 'howm-reminder-tomorrow-face dummy-region-color)

	(set-face-foreground 'howm-mode-title-face highlight-font-color-2)
	(set-face-foreground 'moccur-face "orange4")
	(set-face-background 'moccur-face dummy-region-color)
	(set-face-underline-p 'moccur-face t)
	(set-face-foreground 'moccur-edit-done-face "gray10")
	(set-face-background 'moccur-edit-done-face dummy-region-color)
	(set-face-foreground 'moccur-edit-face "orange4")
	(set-face-background 'moccur-edit-face dummy-region-color)

  ;;-------------------------------
  ;; frame setting
  ;;-------------------------------
  ;; default frame setting
  (defvar my-frame-setting-list
    (list
		 '(foreground-color . "black")
     '(background-color . "white")
     '(cursor-color . "orange")
     '(cursor-type . box)
     '(cursor-height . 4)
     '(mouse-color . "white")
     '(border-color . "black")
     '(vertical-scroll-bars . 'nil)
     '(width . 260)
     '(height . 160)
     '(left . 0)
     '(top . 0)
     '(alpha . 100)
     ))

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
  (global-font-lock-mode 1) 
  (setq font-lock-maximum-decoration t)

  ;;
  (setq frame-title-format
        (format "emacs %s : %%f %s"
                (system-name)
                (emacs-version)))

  (which-function-mode 1)
  (column-number-mode t)
  (line-number-mode t)
  (set-scroll-bar-mode nil)
	;; (set-scroll-bar-mode 'right)
  (blink-cursor-mode 0)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
	(setq frame-alpha-lower-limit 0.01)

  (when (my-is-windows)
    (add-hook 'mw32-ime-on-hook
							(function (lambda () (set-cursor-color "Pink"))))
    (add-hook 'mw32-ime-off-hook
              (function (lambda () (set-cursor-color "White"))))
    (setq-default mw32-ime-mode-line-state-indicator "[--]")
    )

  (when (require 'hl-line nil t)
    (global-hl-line-mode t))

  (require 'paren)
  (show-paren-mode t)
  (custom-set-variables
   '(show-paren-ring-bell-on-mismatch t)
   '(show-paren-style 'mixed)
   )
	
	;;
  (when (require 'highlight-parentheses nil t)
		(highlight-parentheses-mode)
		(custom-set-variables
		 '(hl-paren-background-colors '("#eee0d0"))
		 '(hl-paren-colors '("firebrick2" "IndianRed2" "IndianRed3" "IndianRed4"))
		 ))
	
  ;;-------------------------------
  ;; disable truncate-line in text file.
  ;;-------------------------------
  (add-hook 'find-file-hook
            '(lambda ()
               (unless (string-match "\\.txt$" (buffer-file-name))
                 (setq truncate-lines t)
                 (setq truncate-partial-width-windows t))
               ))

  ;;-------------------------------------
  ;; 
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

  (set-face-foreground 'font-lock-regexp-grouping-backslash "#999")
  (set-face-foreground 'font-lock-regexp-grouping-construct "#999")
  )

(defun my-set-default-color-theme ()
	"初期化のタイミングでうまく設定できないので初期化後に設定するために
コマンド化しておく"
	(interactive)
	(color-theme-solarized-light))

(global-set-key (kbd "C-l C-o C-e") 'my-set-default-color-theme)

(setq mode-line-remote "")
(setq mode-line-mule-info "")
(setq mode-line-client "")
(setq mode-line-process "")
(setq mode-line-position "")

;; the current day and date are displayed as well.
(setq display-time-format "%Y/%m/%d %a %R")
(setq display-time-day-and-date t)
(display-time)

(provide 'init-theme)
