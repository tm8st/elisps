;;; init.el --- init emacs

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init
;; creation time: Sun May  2 00:04:30 2010
;;; Commentary:

;;; Code:

;; "void variable" エラー対策
(defvar warning-suppress-types nil)

;; コンパイル用環境の設定 パスを変える場合はここと下のファイルの中の変数の値を変える必要がある
(load "~/elisps/init/init-compile-env.el")

;;;-------------------------------
;;; emacs-settings
;;;-------------------------------
(defvar my-is-use-emacs-settings nil)

(when my-is-use-emacs-settings

  (require 'cl)

  (unless my-initialized
    (progn
      (defun update-emacs-settings-site-dir (dir)
	"add dir and subdirectories of it to load-path"
	(let ((dirs (remove-if-not #'file-directory-p
				   (directory-files dir t "^[^.]"))))
	  (dolist (d dirs)
	    (update-emacs-settings-site-dir d))
	  (setq load-path (cons dir load-path))))

      (update-emacs-settings-site-dir "/Users/mys/emacs-settings/emacs.d")

      (load "/Users/mys/emacs-settings/init.el")
      (load-emacs-settings "/Users/mys/emacs-settings")))
  )

;;;-------------------------------
;;; path add
;;;-------------------------------
(defvar exec-path-list
  (list
   "~/bin"
   "/usr/bin" "/usr/sbin" "/sbin" "/sw/bin" "/sw/sbin"
   "/usr/local/bin"
   "/opt/local/bin" "/opt/local/sbin"
   (when (my-is-windows)
     "c:/cygwin/bin"
     )
   (when (my-is-mac)
     "/usr/X11/bin" "/usr/X11R6/bin"
     "/softwares/scala/bin"
     "~/softwares"
     )))

(defun add-to-exec-path (dir)
  ""
  (if (eq dir nil)
      nil
    (add-to-list 'exec-path (expand-file-name dir))))

(mapc 'add-to-exec-path exec-path-list)

;;;----------------------------------------
;;; loadpath add all ~/elisps subdirs
;;;----------------------------------------
(defvar my-default-load-path nil)
;; (defvar my-default-load-path load-path)

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
  (if (member (expand-file-name d) my-default-load-path) nil
    (my-byte-recompile-directory d)))

;; (unless my-initialized
;;  (add-to-list 'load-path "~/elisps/emacswikipages" t))

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
     "init-basic.el"
     "init-misc.el"
     "init-linum.el"
     "init-my-misc.el"

     "init-keybindings.el"

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
     "init-theme.el"
     "init-shell.el"
     "init-skk.el"
     "init-migemo.el"

     ;; "init-window.el"

     ;; "init-test.el"
     ))

	(when my-use-shecme-mode
		(add-to-list 'init-load-elisp-list "init-scheme.el")
		)
	(when my-use-scala-mode
		(add-to-list 'init-load-elisp-list "init-scala.el")
		)
	(when my-use-haskell-mode
		(add-to-list 'init-load-elisp-list "init-haskell.el")
		)
	(when my-use-org-mode
		(add-to-list 'init-load-elisp-list "init-org.el")
		)

  (defun load-elisp (file) "" (load file t nil))

  (mapc 'load-elisp init-load-elisp-list)
  )

(setq my-initialized t)

(provide 'init)