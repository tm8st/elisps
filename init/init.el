;;; init.el --- init emacs

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init
;; creation time: Sun May  2 00:04:30 2010
;;; Commentary:

;;; Code:

;; Seed the random-number generator
(random 1000000)

;; garbage collectionの頻度を減らして、速度向上
(setq gc-cons-threshold (* gc-cons-threshold 10))

;; turn off mouse interface.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; set elisps directory path.
(setq my-elisps-directory (getenv "ELISPDIR"))
(if (eq my-elisps-directory nil)
    (progn
      ;; not found ELISPDIR envvar set default elisps path.
      (setq my-elisps-directory "~/elisps")
      (message (concat "init elisp dir is nil, set default " my-elisps-directory ".")))
  (message (concat "init elisp dir is " my-elisps-directory ".")))

;; "void variable" エラー対策
(defvar warning-suppress-types nil)

;; Windows環境で定義されていないとエラーがでるので定義しておく
(unless (fboundp 'string-to-char-list)
  (defun string-to-char-list (str)
    (string-to-list str)))

(defun my-load-elisp (path)
  "safety and more infomation load function."
  (message (concat "start load: " path))
  (load path t)
  )

;; コンパイル用環境の設定elispのパス
;; パスを変える場合はこことinit-compile-envの中の変数の値を変える必要がある
(my-load-elisp (concat my-elisps-directory "/init/init-compile-env.el"))

;;;-------------------------------
;;; add path to exec path.
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
;;; add loadpath all ~/elisps subdirs.
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

(add-to-list 'load-path "~/emacswiki.org" t)

;;----------------------------------------
;; unset key bindings for key-binding-prefix.
;;----------------------------------------
(global-unset-key (kbd "C-q"))
(global-unset-key (kbd "C-l"))

;;;-------------------------------
;;; load some init elisps.
;;;-------------------------------
(when use-customize

  (my-load-elisp "init-compile-env.el")
  (my-load-elisp "init-private.el")
  (my-load-elisp "private.el")

  ;;----------------------------------------
  ;; init elisp list.
  ;;----------------------------------------
  (setq init-load-elisp-list
				(list
				 "init-keybindings.el"

				 "init-basic.el"
				 "init-misc.el"
				 "init-my-misc.el"

				 "init-dired.el"

				 "init-howm.el"
				 "init-org.el"

				 ;; ;; program lang
				 "init-elisp.el"
				 "init-c-mode.el"
				 "init-ruby.el"

				 ;; "init-lua.el"
				 ;; "init-go.el"

				 "init-complete.el"
				 "init-yasnippet.el"

				 "init-shell.el"

				 "init-anything.el"
				 "init-gtags.el"
				 "init-popups.el"
				 "init-skk.el"
				 "init-theme.el"
         "init-vcs.el"
         "init-calfw.el"
	
				 ;; ;; "init-migemo.el"
         ;; "init-view-mode.el"
  	
				 ;; ;; "init-window.el"
				 ;; ;; "init-test.el"
				 ))

  ;; 環境によってOn/Offする設定ファイル
  (when my-use-shecme-mode
    (add-to-list 'init-load-elisp-list "init-scheme.el"))
  (when my-use-scala-mode
    (add-to-list 'init-load-elisp-list "init-scala.el"))
  (when my-use-haskell-mode
    (add-to-list 'init-load-elisp-list "init-haskell.el"))
  (when my-use-twitter-mode
    (add-to-list 'init-load-elisp-list "init-twitter.el"))

  (mapc 'my-load-elisp init-load-elisp-list)
  )

(setq my-initialized t)
  
;; initialized notify by growl.
(when (my-require 'tm8st-growl)
  (add-hook 'emacs-startup-hook
            (lambda () (tm8st-growl-notify (concat "\"Emacs Initialized." "\"")))))

(add-hook 'emacs-startup-hook 'my-set-default-color-theme)

(add-hook 'after-init-hook
          (lambda ()
            (message "init time: %d msec"
                     (+ (* (- (nth 1 after-init-time) (nth 1 before-init-time)) 1000)
                        (/ (- (nth 2 after-init-time) (nth 2 before-init-time)) 1000)))))

(provide 'init)
