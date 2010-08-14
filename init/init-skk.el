;;; init-skk.el --- skk init

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: skk, init
;; creation time: Sun Jul  4 16:13:43 2010
;;; Commentary:

;;; Code:

(customize-set-value 'skk-kakutei-key "\C-o")
;; (customize-set-value 'skk-kakutei-key "\C-o")
;; (customize-set-value 'skk-kakutei-key "\C-m")
(customize-set-value 'skk-show-inline t)
;; (customize-set-value 'skk-show-inline 'vertical)
(customize-set-value 'skk-auto-insert-paren t)

(when my-initialized
  (add-hook 'find-file-hook
	    '(lambda ()
	       (skk-mode t)
	       (skk-latin-mode-on)
	       )))

;; (customize-set-value 'skk-show-tooltip nil)
;; (customize-set-value 'skk-tooltip-y-offset -30)
;; (customize-set-value 'skk-tooltip-parameters
;; 		     '((foreground-color . "navy blue")
;; 		       (background-color . "alice blue")
;; 		       (border-color . "royal blue")
;; 		       (border-width . 2)))
(customize-set-value 'skk-use-color-cursor t)
;; SKK ���[�h���I�t�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�J�[�\���̂���Y�� �t���[���ɂ�����W���̃J�[�\���F���g���܂��B
;; (customize-set-value 'skk-cursor-default-color t)

;; ���ȃ��[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "coral4" �܂��� "pink" ��p���܂��B
(customize-set-value 'skk-cursor-hiragana-color "pink")
 
;; �J�i���[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "forestgreen" �܂��� "green" ��p���܂��B
(customize-set-value 'skk-cursor-katakana-color "red")
;; skk-cursor-katakana-color

;; �A�X�L�[���[�h�ł��邱�Ƃ������J�[�\���F�B�W���ł́A�w�i�̖��Âɂ�� "ivory4" �܂��� "gray" ��p���܂��B
(customize-set-value 'skk-cursor-latin-color "ivory")

(require 'skk)

(global-set-key (kbd "C-l C-h") 'skk-mode)
(setq skk-tut-file "~/elisps/external/ddskk-20100704/etc/SKK.tut")

;; (unless skktut-latin-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map "\C-j" 'skk-kakutei)
    ;; (setq skktut-latin-mode-map map)))

;; (global-set-key "\C-xj" 'skk-auto-fill-mode)
;; (global-set-key "\C-xt" 'skk-tutorial)

;; ;; Specify dictionary location
;; (setq skk-large-jisyo "~/share/skk/SKK-JISYO.L")
;; ;; Specify tutorial location
;; (setq skk-tut-file "~/share/skk/SKK.tut")

;; (add-hook 'isearch-mode-hook
;; 	  (function (lambda ()
;; 		      (and (boundp 'skk-mode) skk-mode
;; 			   (skk-isearch-mode-setup)))))

;; (add-hook 'isearch-mode-end-hook
;; 	  (function
;; 	   (lambda ()
;; 	     (and (boundp 'skk-mode) skk-mode (skk-isearch-mode-cleanup))
;; 	     (and (boundp 'skk-mode-invoked) skk-mode-invoked
;; 		  (skk-set-cursor-properly)))))

(provide 'init-skk)