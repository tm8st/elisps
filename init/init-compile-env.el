;;; init-compile-env.el --- my elisp compile env setting.

;; Copyright (C) 2010 tm8st

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
(defvar toggle-debug-on-signal t)
(defvar debug-on-error t)
(defvar debug-on-quit t)

(defvar use-customize t)
(defvar use-gui-setting (and use-customize t))
(defvar use-font-setting (and use-gui-setting t))
(defvar use-misc-setting (and use-customize t))
(defvar my-force-recompile-elisps t)
(defvar file-cache-path nil)
(defvar my-initialized nil)
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