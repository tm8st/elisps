;;; init.el --- init emacs

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init
;; creation time: Sun May  2 00:04:30 2010
;;; Commentary:

;;; Code:

;; コンパイル用環境の設定 パスを変える場合はここと下のファイルの中の変数の値を変える必要がある
(load "~/elisps/init/init-compile-env.el")

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
     )
   ))

(defun add-to-exec-path (dir)
  ""
  (if (eq dir nil)
      nil
    (add-to-list 'exec-path (expand-file-name dir))))

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
  (let (save-abbrevs) (byte-recompile-directory (expand-file-name dir) nil my-force-recompile-elisps)))

(dolist (d load-path)
  (if (member (expand-file-name d) my-default-load-path) nil
    (my-byte-recompile-directory d)
    )
  )

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
     "init-howm.el"
     "init-dired.el"
     "init-shell.el"
     "init-elisp.el"
     "init-c-mode.el"
     "init-ruby.el"
     "init-lua.el"
     "init-go.el"
     "init-scheme.el"
     "init-scala.el"
     "init-complete.el"
     "init-yasnippet.el"
     "init-gtags.el"
     "init-anything.el"
     "init-org.el"
     "init-popups.el"
     "init-theme.el"
     "init-keybindings.el"
 
     ;; "init-migemo.el"
     ;; "init-test.el"
     ))
  
  (defun load-elisp (file) "" (load file t nil))

  (mapc 'load-elisp init-load-elisp-list)
  )

(setq my-initialized t)

(provide 'init)