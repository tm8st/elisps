;;; init-scheme.el --- scheme setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, scheme
;; creation time: Wed Apr 28 00:56:00 2010
;;; Commentary:

;;; Code:

;; 実行環境
(setq scheme-program-name "gosh")
(setq scheme-info-name "/usr/local/info/gauche-refj.info.gz")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
(require 'cmuscheme)

(defun scheme-other-frame ()
  "Run scheme on other frame"
  (interactive)
  (switch-to-buffer-other-frame
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(defun sheme-info ()
  (interactive)
  (switch-to-buffer-other-frame
   (get-buffer-create "*info*"))
  (info scheme-info-name))

(define-key global-map
  (kbd "C-l C-") 'scheme-other-frame)

(global-set-key (kbd "C-l C-@ C-w") 'scheme-other-window)
(global-set-key (kbd "C-l C-@ C-f") 'scheme-other-frame)
(global-set-key (kbd "C-l C-@ C-j") 'scheme-info)

;; (setq exe-path (cons (expand-file-name "/Applications/PLT Scheme v4.2.3/bin/") exec-path))
;; (setq scheme-program-name "MzScheme.exe")	  ;;スキーム処理系
;; (autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)
;; (define-key global-map (kbd "C-c s") 'run-scheme) ; C-c s で emacs 上で MzScheme が走るようにする

(provide 'init-scheme)
