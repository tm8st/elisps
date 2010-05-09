;;; init-complete.el --- my complete setting.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, complete

;;; Commentary:

;;; Code:

(require 'complete) ;; 強力な補完機能を使う
(partial-completion-mode 1)
(icomplete-mode t) ;; 補完可能なものを随時表示

;;;-------------------------------------
;;;auto-complete setting
;;;-------------------------------------
(require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
(global-auto-complete-mode t)
(customize-set-value 'ac-dictionary-directories (list "~/elisps/external/complete/m2ym-auto-complete-2c75fd1/dict"))

(setq ac-auto-start 3)
(customize-set-value 'ac-ignore-case 'smart)
(customize-set-value 'ac-candidate-limit 1000)
(customize-set-value 'ac-use-fuzzy 'nil)
(customize-set-value 'ac-use-comphist 'nil)
(customize-set-value 'ac-use-quick-help 'nil)
(customize-set-value 'ac-delay 0.2)

(global-set-key (kbd "C-o") 'ac-start)
;; (global-set-key (kbd "C-q C-c") 'auto-complete-mode)

(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-o") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-u") 'ac-stop)
(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-j") 'ac-complete)

;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-@") 'ac-complete-with-anything)

(require 'popup)
(when use-gui-setting
  (set-face-foreground 'ac-candidate-face "White")
  (set-face-foreground 'ac-selection-face "Pink")
  (set-face-background 'ac-candidate-face "gray30")
  (set-face-background 'ac-selection-face "gray30")

  (set-face-foreground 'popup-menu-face "White")
  (set-face-background 'popup-menu-face "gray30")
  (set-face-background 'popup-menu-selection-face "gray30")
  (set-face-foreground 'popup-menu-selection-face "Pink")
  (set-face-background 'popup-menu-selection-face "gray30")
  )

;;;-------------------------------------
;;;標準補間機能のカスタマイズ completion
;;;-------------------------------------
;;key binding
(define-key completion-list-mode-map "\C-n" 'next-completion)
(define-key completion-list-mode-map "\C-f" 'next-completion)
(define-key completion-list-mode-map "\C-p" 'previous-completion)
(define-key completion-list-mode-map "\C-b" 'previous-completion)
(define-key completion-list-mode-map "\C-m" 'my-choose-completion)

(defun my-choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	    (base-size completion-base-position))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
	(if (null beg)
	    (error "No completion here"))
	(setq beg (previous-single-property-change beg 'mouse-face))
	(setq end (or (next-single-property-change end 'mouse-face) (point-max)))
	;; (setq pletion (buffer-substring-no-properties beg end))
	(delete-completion-window)
	(choose-completion-string completion buffer base-size)))

;; 動的補完で無視する要素の正規表現
(customize-set-value 'dabbrev-abbrev-skip-leading-regexp "-")

(provide 'init-complete)
