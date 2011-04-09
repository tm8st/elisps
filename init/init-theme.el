
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
  (customize-set-variable 'yalinum-line-number-length-min 5)
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
    )

	(defvar default-background-color "#fdf6e3")
	
  (set-face-foreground 'yalinum-face "gray50")
  (set-face-foreground 'yalinum-bar-face "Pink3")
	
	(set-face-background 'yalinum-face default-background-color)
	(set-face-background 'yalinum-bar-face default-background-color)

  (require 'jaunte)
  (set-face-foreground 'jaunte-hint-face "pink3")
  (set-face-background 'jaunte-hint-face "gray50")  
	)

(when use-gui-setting

  ;;-------------------------------
  ;; frame setting
  ;;-------------------------------
  ;; default frame setting
  (defvar my-frame-setting-list
    (list
     '(background-color . "#006000")
     '(foreground-color . "gray75")
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

  (when (my-is-windows)
    (add-hook 'mw32-ime-on-hook
							(function (lambda () (set-cursor-color "Pink"))))
    (add-hook 'mw32-ime-off-hook
              (function (lambda () (set-cursor-color "White"))))
    (setq-default mw32-ime-mode-line-state-indicator "[--]")
    )

  (when (require 'hl-line nil t)
    ;; (defface;;  my-hl-line-face
    ;;   '((t (:background "gray10")))
    ;;   ""
    ;;   :group 'yalinum)
		
    ;; ;; (customize-set-value 'hl-line-face 'underline)
    ;; (customize-set-value 'hl-line-face 'my-hl-line-face)
    ;; ;; (customize-set-value 'hl-line-face 'highlight)

    (global-hl-line-mode t)
    )

  ;;-------------------------------
  ;;  
  ;;-------------------------------
  (require 'paren)
  (show-paren-mode t)
  (custom-set-variables
   '(show-paren-ring-bell-on-mismatch t)
   '(show-paren-style 'mixed)
   )
		 
	;;
  (require 'highlight-parentheses)
  (highlight-parentheses-mode)
  (custom-set-variables
	 '(hl-paren-background-colors '("#fdf6e3"))
	 '(hl-paren-colors '("red" "orange" "green" "bule" "black"))
	 ;; '(hl-paren-foreground-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
	 ;; '(hl-paren-foreground-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
   ;; '(hl-paren-background-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
   ;; '(hl-paren-colors '("orange" "blue" "yellow" "red"))
   ;; '(hl-paren-background-colors nil)
   )
	
  ;;-------------------------------
  ;; disable truncate-line in text file.
  ;;-------------------------------
  (add-hook 'find-file-hook
            '(lambda ()
               (unless (string-match "\\.txt$" (buffer-file-name))
                 (setq truncate-lines t)
                 (setq truncate-partial-width-windows t))
               )
            )

  (set-scroll-bar-mode nil)
  ;; (set-scroll-bar-mode 'right)
  (blink-cursor-mode 0)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
	(setq frame-alpha-lower-limit 0.01)

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

(require 'color-theme-solarized)
(color-theme-solarized-light)

(provide 'init-theme)
