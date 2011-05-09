;;; init-compile-env.el --- my elisp compile env setting.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, funcs, compile, env

;;; Commentary:

;;; Code:

;; check os-type function
(defun my-is-mac () "check run emacs on mac."
  (not
   (eq
    (string-match "apple" (emacs-version))
    nil)))

(defun my-is-windows () "check run emacs on windows."
  (not (my-is-mac)))

(defun my-not-windows () "check run emacs on not windows."
  (not (my-is-windows)))

;;;----------------------------------------
;;; customize setting
;;;----------------------------------------
(defvar my-initialized nil)

(setq toggle-debug-on-signal t)
(setq debug-on-error t)
(setq debug-on-quit t)

(defvar use-customize t)
(defvar use-gui-setting (and t use-customize))
(defvar use-font-setting t)
(defvar use-misc-setting (and t use-customize))
(defvar my-force-recompile-elisps nil)
(defvar file-cache-path nil)
(defgroup my nil "" :group 'my)

(defvar my-is-use-emacs-settings t)
(defvar my-use-shecme-mode t)
(defvar my-use-scala-mode t)
(defvar my-use-haskell-mode t)
(defvar my-use-org-mode t)

(defvar my-elisp-path "~/elisps")

(unless my-initialized
  (add-to-list 'load-path my-elisp-path)
  (add-to-list 'load-path (concat my-elisp-path "/init"))
  (add-to-list 'load-path "~/emacswiki.org" t)
  )

(defvar init-load-elisp-list nil)

;;----------------------------------------
;; original key-binding-prefix
;;----------------------------------------
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-l"))

(require 'init-private)
(require 'private) ;; 値の設定(非公開ファイル)

(provide 'init-compile-env)
