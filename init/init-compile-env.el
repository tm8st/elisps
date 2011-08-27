;;; init-compile-env.el --- my elisp compile env setting.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, funcs, compile, env

;;; Commentary:

;;; Code:

;;; Profile
(defvar my-required-package-list nil)
(defun my-require (package_name)
  (condition-case err
      ((lambda ()
         (require package_name)
         (message "my-require ok: %s" package_name)
         (add-to-list 'my-required-package-list package_name)
         t))
    (error
     (message "my-require ng: %s" (error-message-string err))
     nil
     )))

;; check os-type function.
(defun my-is-mac () "check run emacs on mac."
  (eq system-type 'darwin))

(defun my-is-windows () "check run emacs on windows."
  (or
   (eq system-type 'windows-nt)
   (eq system-type 'ms-dos)))

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
(defvar use-font-setting (and t use-customize))
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

(my-require 'init-private)
(my-require 'private) ;; 値の設定(非公開ファイル)

(add-to-list 'load-path (concat my-dropbox-directory "Elisp"))


(provide 'init-compile-env)
