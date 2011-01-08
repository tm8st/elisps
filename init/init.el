;;; init.el --- inmyit emacs

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init
;; creation time: Sun May  2 00:04:30 2010
;;; Commentary:

;;; Code:

;; garbage collectionの頻度を減らして、速度向上 デフォルトは400000
(setq gc-cons-threshold (* gc-cons-threshold 10))

;; turnoff mouse interface.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq my-elisps-directory (getenv "ELISPDIR"))
(message (concat "init elisp dir is " my-elisps-directory "."))
(setq my-elisps-directory "~/elisps")

;; "void variable" エラー対策
(defvar warning-suppress-types nil)

;; Windows環境で定義されていないとエラーがでるので定義しておく
(defun string-to-char-list (str)
  (string-to-list str))

;; コンパイル用環境の設定 パスを変える場合はここと下のファイルの中の変数の値を変える必要がある
(load (concat my-elisps-directory "/init/init-compile-env.el"))

;;;-------------------------------
;;; emacs-settings
;;; いくつか不具合がでたのでちょっととめる
;;;-------------------------------
;; (when my-is-use-emacs-settings
;;   (require 'cl)

;;   (unless my-initialized
;;     (progn
;;       (defun update-emacs-settings-site-dir (dir)
;;      "add dir and subdirectories of it to load-path"
;;      (let ((dirs (remove-if-not #'file-directory-p
;;                                 (directory-files dir t "^[^.]"))))
;;        (dolist (d dirs)
;;          (update-emacs-settings-site-dir d))
;;        (setq load-path (cons dir load-path))))

;;       (update-emacs-settings-site-dir "/Users/mys/emacs-settings/emacs.d")

;;       (load "/Users/mys/emacs-settings/init.el")
;;       (load-emacs-settings "/Users/mys/emacs-settings"))))

;;;-------------------------------
;;; path add
;;;-------------------------------
(defvar exec-path-list
  (list
   "~/bin"
   "/usr/bin" "/usr/sbin" "/sbin" "/sw/bin" "/sw/sbin"
   "/usr/local/bin"
   "/opt/local/bin" "/opt/local/sbin"
         ))

(defun add-to-exec-path (dir)
  ""
  (if (eq dir nil)
      nil
    (add-to-list 'exec-path (expand-file-name dir))))

(mapc 'add-to-exec-path my-exec-path)
(mapc 'add-to-exec-path exec-path-list)

;;;----------------------------------------
;;; loadpath add all ~/elisps subdirs
;;;----------------------------------------
(defvar my-default-load-path load-path)

(let ((dir (expand-file-name my-elisp-path)))
  (if (member dir load-path) nil
    (setq load-path (cons dir load-path))
    (let ((default-directory dir))
      (load (expand-file-name "subdirs.el") t t t))))

(defun my-byte-recompile-directory (dir)
  (interactive "DByte recompile directory:")
  (let (save-abbrevs)
    (if my-force-recompile-elisps
        (byte-recompile-directory (expand-file-name dir) 0 my-force-recompile-elisps)
      (byte-recompile-directory (expand-file-name dir)))))

(dolist (d load-path)
  (unless (eq d nil)
    (if (member (expand-file-name d) my-default-load-path) nil
      (my-byte-recompile-directory d))))

;; (dolist (d load-path)
;; (if (member (expand-file-name d) my-default-load-path) nil
;;   (my-byte-recompile-directory d)))


(add-to-list 'load-path "~/emacswiki.org" t)

;;;-------------------------------
;;; start customize
;;;-------------------------------
(when use-customize

  ;;----------------------------------------
  ;; init etc
  ;;----------------------------------------
  (setq init-load-elisp-list
    (list
     "init-compile-env.el"
     "init-private.el"
     "private.el"

     "init-keybindings.el"

     "init-basic.el"
     "init-misc.el"
     "init-my-misc.el"

     "init-howm.el"
     "init-dired.el"

     ;; program lang
    "init-elisp.el"
    "init-c-mode.el"
    "init-ruby.el"
    "init-lua.el"
    "init-go.el"

    "init-complete.el"
    "init-yasnippet.el"
    "init-gtags.el"
    "init-anything.el"
    "init-popups.el"
    "init-shell.el"
    "init-skk.el"
    "init-migemo.el"

     "init-theme.el"

     ;; "init-window.el"
     ;; "init-test.el"
     ))

  ;; 環境別の設定ファイル
  (when my-use-shecme-mode
    (add-to-list 'init-load-elisp-list "init-scheme.el"))
  (when my-use-scala-mode
    (add-to-list 'init-load-elisp-list "init-scala.el"))
  (when my-use-haskell-mode
    (add-to-list 'init-load-elisp-list "init-haskell.el"))
  (when my-use-org-mode
    (add-to-list 'init-load-elisp-list "init-org.el"))
  (when my-use-twitter-mode
    (add-to-list 'init-load-elisp-list "init-twitter.el"))

  (defun my-init-load-elisp (file) ""
    (load file t nil)
    )

  (mapc 'my-init-load-elisp init-load-elisp-list)
  )

(setq my-initialized t)

(provide 'init)
