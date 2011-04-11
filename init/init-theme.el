
;;; Code:

(eval-when-compile (require 'cl))

(when use-font-setting

  ;;-------------------------------
  ;; Font setting
  ;;-------------------------------
  (defvar my-font-size-base 100)
  (cond
   ((my-is-windows) (setq my-font-size-base 135))
   ((my-is-mac) (setq my-font-size-base 230)))

  (when (and use-font-setting (my-is-windows)
             (set-face-attribute 'default nil
																 :family "VL ゴシック"
                                 :height my-font-size-base)))
    
  (when (and use-font-setting (my-is-mac)
             (set-face-attribute 'default nil
																 :family "Inconsolata"
																 ;; :family "MiguMix 1M"
																 ;; :family "Osaka"
																 ;; :family "AppleGothic"
																 ;; :family "Helvetica Neue"
																 ;; :family "Helvetica"
																 ;; :family "Hiragino Kaku Gothic Std"
																 ;; :family "VL PGothic"
																 ;; :family "VL Gothic"
                                 ;; :family "Hiragino Kaku Gothic ProN"
                                 :height my-font-size-base)))

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

	(require 'yalinum)
  (set-face-foreground 'yalinum-face defaul-font-color)
  (set-face-foreground 'yalinum-bar-face defaul-font-color)
	(set-face-background 'yalinum-face default-background-color)
	(set-face-background 'yalinum-bar-face dummy-region-color)

	(require 'yafastnav)
	(set-face-foreground 'yafastnav-shortcut-key-face-type highlight-font-color)
	(set-face-background 'yafastnav-shortcut-key-face-type highlight-background-color)

  (require 'jaunte)
  (set-face-foreground 'jaunte-hint-face highlight-font-color-2)
  (set-face-background 'jaunte-hint-face highlight-background-color)

	(setq my-popup-cadidate-color defaul-font-color)
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
     '(cursor-type . bar)
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

  (display-time)
  (setq display-time-day-and-date t)
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
		 '(hl-paren-background-colors '(dummy-region-color))
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

(color-theme-solarized-light)

(provide 'init-theme)
