;;; init-misc.el --- misc customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

(require 'saveplace)
(require 'ansi-color)
(require 'tramp)

;; minibufferでC-wで前の単語を削除
(define-key minibuffer-local-completion-map (kbd "C-w") 'my-delete-backward-word)

;;; for Kayac Emacs
;; fullscreen
(when (my-is-mac)
  (global-set-key (kbd "M-f") 'ns-toggle-fullscreen)
  )

;;;--------------------------------
;;; ffap find-file でURLも開ける
;;;--------------------------------
(require 'ffap)

(ffap-bindings)
(setq ffap-newfile-prompt t)
(setq ffap-rfc-path "http://www.minokasago.org/labo/RFC/rfc%s-jp.html")
;;(setq ffap-rfc-path "http://www.ring.gr.jp/archives/doc/RFC/rfc%s.txt")
(setq ffap-dired-wildcards "*")
(setq ffap-machine-p-known 'accept)
(setq ffap-kpathsea-depth 8)

;;;--------------------------------------------------------------------------------
;;; color-moccur anything-color-moccurへ移項
;;;--------------------------------------------------------------------------------
(require 'color-moccur)
;; (global-set-key (kbd "C-q C-l C-s") 'occur-by-moccur) ;;現在バッファを検索
;; (global-set-key (kbd "C-q C-l C-g") 'moccur-grep)
;; (global-set-key (kbd "C-q C-l C-f") 'moccur-grep-find)
;; (global-set-key (kbd "C-q C-l C-d") 'dmoccur)
;; (global-set-key (kbd "C-q C-l C-c") 'clean-dmoccur-buffers)
;; (global-set-(kbd key " C-q C-l C)-d" '(lambda () (interactive) (progn (dmoccur) (clean-dmoccur-buffers))))

(setq moccur-split-word nil)
(setq *moccur-buffer-name-exclusion-list*
	  '(".+TAGS.+" "*Completions*" "*Messages*"
		"newsrc.eld" " *migemo*" ".bbdb"))

(setq dmoccur-list
      '(
	("cd" default-directory (".*") nil)
	))

(set 'dmoccur-use-list t)
(set 'dmoccur-maximum-size 1000)

(require 'moccur-edit)

;;;-------------------------------------
;;; 自動キーボードマクロ
;;;-------------------------------------
(defconst *dmacro-key* "\C-]" "繰返し指定キー")
(global-set-key *dmacro-key* 'dmacro-exec)
(autoload 'dmacro-exec "dmacro" nil t)

;;;-------------------------------------
;;; windows の関連付けでファイルを開く
;;;-------------------------------------
(when (my-is-windows)

  (defvar fiber-exe "cygstart.exe")

  (defun buffer-fiber-exe ()
    (interactive)
    (let ((file (buffer-file-name)))
      (cond
       ((string= major-mode 'dired-mode)
	(if (string-match "^([a-z]:)/$" default-directory)
	    (start-process "explorer" "diredfiber" "explorer.exe"
			   (match-string 1  default-directory))
	  (start-process "explorer" "diredfiber" "explorer.exe"
			 (anything-c-w32-pathname-transformer
			  (directory-file-name
			   default-directory)))))
       ((and buffer-file-name
	     (file-exists-p buffer-file-name))
	(start-process fiber-exe "diredfiber" fiber-exe
		       buffer-file-name))
       ((not file)
	(error
	 "現在のバッファはファイルではありません"))
       ((file-directory-p file)
	(start-process
	 "explorer" "diredfiber" "explorer.exe"
	 (anything-c-w32-pathname-transformer file)))
       ((file-exists-p file)
	(start-process
	 "fiber" "diredfiber" fiber-exe file))
       ((not (file-exists-p file))
	(error "ファイルが存在しません")))))

  (global-set-key (kbd"C-q C-f C-w") 'buffer-fiber-exe)
  )

;;;-------------------------------------
;;; emacs-keybind
;;;-------------------------------------
;; (require 'emacs-keybind)
;; ;; 解析に使用するrubyスクリプトのパス(emacs-keybind.elと同じ場所にあります)
;; (setq emacs-keybind-program-file "~/elisps/misc/emacs-keybind/emacs_keybind.rb")
;; (defvar emacs-keybind-work-dir "~/.emacs.d")
;; ;; キーボード種類(ascii or japanese)
;; (setq emacs-keybind-keyboard-kind "japanese")
;; emacs-keybind.el が自動で生成するファイルの置き場所
;; (setq emacs-keybind-program-file "~/.emacs.d/emacs-keybind/")
;; (setq emacs-keybind-work-dir "c:/home/emacs_keybind")

;;;-------------------------------
;;; hatena-mode
;;;-------------------------------
(require `hatena-mode)
(require `hatena-vars)
(setq hatena-usrid my-hatena-usrid)
(setq hatena-plugin-directory "~/elisps/hatena")
(setq hatena-use-file t)
;; (setq hatena-default-coding-system 'utf-8)

;;;-------------------------------
;;; ediff
;;;-------------------------------
(require 'ediff)
(set-face-foreground 'ediff-odd-diff-C "black") 	;;衝突箇所
(set-face-background 'ediff-odd-diff-C "gray25") 	;;衝突箇所
(set-face-foreground 'ediff-current-diff-C "black") ;;選択中衝突箇所
(set-face-background 'ediff-current-diff-C "gray30") ;;選択中衝突箇所
(set-face-foreground 'ediff-fine-diff-C "black") ;;選択中衝突箇所見出し
(set-face-background 'ediff-fine-diff-C "slate gray") ;;選択中衝突箇所見出し

;;;----------------------------------------
;;; buffer list
;;;----------------------------------------
(require 'ibuffer)
(defun ibuffer-visit-buffer-other-window-scroll (&optional down)
  (interactive)
  (let ((buf (ibuffer-current-buffer)))
	(unless (buffer-live-p buf)
	  (error "Buffer %s has been killed!" buf))
	(if (string=
		 (buffer-name (window-buffer (next-window)))
		 (buffer-name buf))
		(if down
			(scroll-other-window-down nil)
		  (scroll-other-window))
	  (ibuffer-visit-buffer-other-window-noselect))))
(defun ibuffer-visit-buffer-other-window-scroll-down ()
  (interactive)
  (ibuffer-visit-buffer-other-window-scroll t))
(define-key ibuffer-mode-map " " 'ibuffer-visit-buffer-other-window-scroll)
(define-key ibuffer-mode-map "b" 'ibuffer-visit-buffer-other-window-scroll-down)

(defadvice ibuffer-forward-line
  (after ibuffer-scroll-page activate)
  (ibuffer-visit-buffer-other-window-scroll))
(defadvice ibuffer-backward-line
  (after ibuffer-scroll-page-down activate)
  (ibuffer-visit-buffer-other-window-scroll-down))

(global-set-key (kbd "C-x b") 'ibuffer)

;;;----------------------------------------
;;; iswitchb
;;;----------------------------------------
(require 'iswitchb)
(iswitchb-mode 1)
;; (iswitchb-default-keybindings)
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "display the selected buffer in the window"
  (when (and
		 (eq iswitchb-method iswitchb-default-method)
		 iswitchb-matches)
	(select-window
	 (get-buffer-window (cadr (buffer-list))))
	(let ((iswitchb-method 'samewindow))
	  (iswitchb-visit-buffer
	   (get-buffer (car iswitchb-matches))))
	(select-window (minibuffer-window))))
;; (defun iswitchb-my-keys ()
;;   "Add my keybindings for iswitchb."
;;   (define-key iswitchb-mode-map "\C-n" 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map "\C-p" 'iswitchb-prev-match)
;;   (define-key iswitchb-mode-map "\C-f" 'iswitchb-next-match)
;;   (define-key iswitchb-mode-map "\C-b" 'iswitchb-prev-match)
;;   )

;;;-------------------------------
;;; twittering-mode
;;;-------------------------------
(require 'twittering-mode)
(setq twittering-username my-twittering-username)
(setq twittering-password my-twittering-password)
(global-set-key (kbd "C-l C-t C-w") 'twittering-mode)

;; Basic key bindings
;;
;;     ‘C-c C-s’ to write a tweet (‘twittering-update-status-interactive’)
;;     ‘j’ to go to the next tweet (‘twittering-goto-next-status’)
;;     ‘k’ to go to the previous tweet (‘twittering-goto-previous-status’)
;;     ‘C-m’ (Enter key) to reply to the current tweet or to open the URL under the cursor (‘twittering-enter’)
;;     ‘C-c C-m’ (C-c Enter) to retweet the current tweet (‘twittering-retweet’)
;;     ‘C-c C-f’ to read your friends’ timeline (‘twittering-friends-timeline’)
;;     ‘C-c C-r’ to read your replies (‘twittering-replies-timeline’)
;;     ‘C-c C-u’ to read your own timeline (‘twittering-user-timeline’)
;;     ‘g’ to refresh the current timeline (‘twittering-current-timeline’)
;;     ‘v’ to view the timeline of the user under the cursor (‘twittering-other-user-timeline’)
;;     ‘V’ to view the timeline of any user (‘twittering-other-user-timeline-interactive’)

;;;-------------------------------
;;; auto byte compile
;;;-------------------------------
(require 'auto-async-byte-compile)

(setq auto-async-byte-compile-init-file "~/elisps/init/init-compile-env.el")
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)

;;;-------------------------------------
;;; 翻訳機能
;;;-------------------------------------
(require 'text-translator)

;; プリフィックスキーを変更する
;; (setq text-translator-mode-pkey-map (kbd "C-i"))
;;(autoload 'text-translator "text-translator" "Text Translator" t)
;;(load "text-translator")

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)

;;(global-set-key "\C-q\C-t\C-t" 'text-translator)
;;(global-set-key "\C-x\C-t\C-t" 'text-translator-translate-last-string)

(global-set-key (kbd "C-q i") 'text-translator-translate-by-auto-selection)
(global-set-key (kbd "C-q e i") 'text-translator-all-by-auto-selection)

(set 'text-translator-default-engine "google.com_enja")
;; (set 'text-translator-default-engine "excite.co.jp_enja")
;; (set 'text-translator-default-engine "excite.co.jp_enja")

;; 改行落ちの回避
(setq text-translator-pre-string-replace-alist nil)
;; 翻訳前テキストを残す
(setq text-translator-leave-string t)

(provide 'init-misc)
