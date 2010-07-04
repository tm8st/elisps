;;; init-basic.el --- basic customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

;;;----------------------------------------
;;; 文字コード
;;;----------------------------------------
;; (set-language-environment 'Japanese)
;; (prefer-coding-system 'utf-8)
;; (set-terminal-coding-system 'japanese-shift-jis)

;; (set-default-coding-systems 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8-auto)
(prefer-coding-system 'sjis)
(set-terminal-coding-system 'sjis)

;; (prefer-coding-system 'euc-jp)

;; (set-default-coding-systems 'euc-jp)
;; (set-buffer-file-coding-system 'euc-jp)
;; (setq file-name-coding-system 'euc-jp)
;; (set-keyboard-coding-system 'euc-jp)
;; (set-clipboard-coding-system 'euc-jp)

;; (prefer-coding-system 'euc-jp)

;; (set-default-coding-systems 'utf-8)
;; (set-buffer-file-coding-system 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-clipboard-coding-system 'utf-8)
;; (set-file-name-coding-system 'utf-8)

;;;----------------------------------------
;;; misc-setting
;;;----------------------------------------
(when use-misc-setting

  ;;--------------------------------
  ;; filecache
  ;;--------------------------------
  (require 'filecache)
  (file-cache-add-directory-list (list "~/"))
  (file-cache-add-directory-list load-path)
  (file-cache-add-directory-list develop-path)
  (file-cache-add-directory-list etc-path)
  (file-cache-add-directory-list file-cache-path)
  
  (define-key minibuffer-local-completion-map
    (kbd "C-q C-i") 'file-cache-minibuffer-complete)

  ;;----------------------------------------
  ;; elisp-install
  ;;----------------------------------------
  (require 'auto-install)
  (custom-set-variables
   '(auto-install-directory "~/elisps/new/")
   '(install-elisp-repository-directory "~/elisps/rep/")
   '(auto-install-update-emacswiki-package-name t)
   )

  ;; C-kで行全体を削除
  (setq kill-whole-line t)
  (setq inhibit-startup-message t) ;;起動画面を表示しない
  (setq enable-recursive-minibuffers t)	;;前のcommandが終了してなくても、新しいcommandを実行可能にする。
  (global-auto-revert-mode t) ;;file が他から変更されたら、自動的に読み込む。
  (delete-selection-mode 1) ;; マーク選択中の編集コマンドの挙動変更
  (set-default 'indicate-empty-lines t)
  ;;  (autoload 'kill-summary "kill-summary" nil t)
  (setq ns-pop-up-frames nil) ;; 新規フレームなし。
  (customize-set-value 'next-line-add-newlines nil) ;; カーソル移動で行を作らない
  
  ;; C-kで行全体を削除
  (setq kill-whole-line t)

  ;; 改行コードを表示
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  
  (require 'saveplace)	;;以前編集していた位置を開く
  (setq-default save-place t)
  (auto-compression-mode t);; 圧縮ファイルを透過的に開く

  (setq indent-tabs-mode t) ;; tab を使うか
  (setq tab-width 4)	;; tab 幅を 4 に設定
  (file-name-shadow-mode t) ;;ファイル名入力時に不用になった部分暗くする
  (setq redisplay-dont-pause t)	 ;; キーリピートにカーソルを追随させる 副作用があるらしい...
  ;;mini buffer での質問に yes/no を入力するのは面倒なのでSPC で yes とする。
  (defalias 'yes-or-no-p 'y-or-n-p)
  (random t) ;; Seed the random-number generator
  (setq undo-outer-limit 100000);; undo の保存限界

  ;; scroll force 1line.
  (setq scroll-conservatively 35
        scroll-margin 0
        scroll-step 1)

  ;; garbage collectionの頻度を減らして、速度向上
  ;; デフォルトは400000
  ;;   (setq gc-cons-threshold 500000)
  (setq gc-cons-threshold 3500000)  ;; あまり大きくするとGCに時間がかかるかも（CVS HEADの値にあわせたMax値。これ以上は意味ないらしい。）

  ;;(setq required-argument t) ;;file の最後は 必ず newline で終わる様にする。
  (setq line-number-display-limit 10000)   ;;表示される最大行数を大きくする。
  (mouse-wheel-mode t) ;;ホイールマウス
  (setq mouse-wheel-follow-mouse t) ;;

  ;; (set-locale-environment nil)	;;Localeに合わせた環境の設定

  (setq scroll-step 1) ;; スクロールを一行ずつにする
  (set-scroll-bar-mode nil)
  (setq completion-ignore-case t) ;; 補完時に大文字小文字を区別しない
  ;; emacs 23用?
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (auto-image-file-mode)
  
  (require 'recentf)
  (recentf-mode 1) ;;最近使った file を記憶させる。
  (custom-set-variables
   '(recentf-max-menu-items 1000)
   '(recentf-max-saved-items 1000)
   )

  (setq-default line-spacing 0)	;; Add 1 pixel between lines
  (recentf-mode)			;; Add menu-item "File--Open recent"

  ;;別のdirectoryにある同一名のfileを開いた時に、
  ;;numberingではなく、directorynameを表示して区別出来る様にする。
  (require 'uniquify)
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")
   '(uniquify-buffer-name-style 'forward)
   )

  ;; file name の TAB 補完する際、拡張子を判別して 色付してくれる。
  ;; (require 'dircolors)

  ;;----------------------------------------
  ;; backup files.
  ;;----------------------------------------
  (setq make-backup-files t)
  (setq make-backup-files t)
  (setq backup-directory-alist
        (cons (cons "\\.*$" (expand-file-name "~/backups"))
              backup-directory-alist))

  ;;----------------------------------------
  ;; auto save.
  ;;----------------------------------------
  ;; (setq auto-save-default t)
  ;; (setq auto-save-interval 256)
  ;; (setq auto-save-timeout 120)
  ;; (setq temporary-file-directory "~/tmp/")
  (require 'auto-save-buffers)
  (customize-set-variable 'auto-save-buffers-exclude-regexp "\\.cpp$\\|\\.h$\\|\\.emacs$|\\.uc$|\\.usf$")
  ;;タイマー設定
  (defvar my-auto-save-buffers-timer nil)
  (unless (eq my-auto-save-buffers-timer nil)
    (cancel-timer my-auto-save-buffers-timer))
  (setq my-auto-save-buffers-timer
        (run-with-idle-timer 10.0 t 'auto-save-buffers))

  ;;----------------------------------------
  ;; 警告時に何もおこらなくする
  ;;----------------------------------------
  ;;visible-bell は目が痛いので消す
  (setq ring-bell-function '(lambda ()))
  ;;beepを消す
  (setq visible-bell t)
  ;; (setq visible-bell nil) ;;visible-bell は目が痛い。

  ;;-------------------------------
  ;; マック用設定
  ;;-------------------------------
  (when (my-is-mac)
    ;; Swap Command-Key, Option-Key
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super))
    )

  ;;----------------------------------------
  ;; ウィンドウズ用設定 
  ;;----------------------------------------
  (when (my-is-windows)
    (setq w32-pass-alt-to-system t) ;; ALTの入力をwindowsに通知するか 最大化、移動用
    (setq w32-phantom-key-code t)
    (setq w32-use-full-screen-buffer t)
    (setq w32-hide-mouse-timeout 1)
    (setq w32-hide-mouse-on-key t)

    ;;cygwin風ディレクリ指定
    (require 'cygwin-mount)
    (cygwin-mount-activate)

    ;;-------------------------------
    ;; IME
    ;;-------------------------------
    ;; (require 'info)
    ;; (set-language-environment "Japanese")
    ;; (w32-ime-initialize)
    ;; (setq default-input-method "MW32-IME")
    ;; (setq-default mw32-ime-mode-line-state-indicator "[--]")
    ;; (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[あ]" "[--]"))
    ;; (require 'iswitchb)
    ;; (iswitchb-default-keybindings)
    ;; (add-to-list 'iswitchb-buffer-ignore "")
    )  
  )

(provide 'init-basic)
