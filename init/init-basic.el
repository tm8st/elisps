;;; init-basic.el --- basic customize.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

;;;----------------------------------------
;;; 文字コード
;;;----------------------------------------
(when (my-is-windows)
  (set-language-environment 'Japanese)
  (prefer-coding-system 'japanese-shift-jis-dos)
  (set-terminal-coding-system 'japanese-shift-jis-dos)
  )

(when (my-is-mac)
  (set-default-coding-systems 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  )

;; terminalで日本語表示がおかしくなったため。
;; (prefer-coding-system 'sjis)
;; (set-terminal-coding-system 'sjis)

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

  ;; 削除ファイルをゴミ箱へ/OS毎のデフォルトディレクトリを使用
  (setq delete-by-moving-to-trash t)

  ;;--------------------------------
  ;; filecache
  ;;--------------------------------
  (require 'filecache)
  (file-cache-add-directory-list (list "~/"))
  (file-cache-add-directory-list load-path)
  (file-cache-add-directory-list exec-path)

  (when file-cache-path
    (file-cache-add-directory-list file-cache-path))
  (when my-etc-path
    (file-cache-add-directory-list my-etc-path))
  (when my-develop-path
    (file-cache-add-directory-list my-develop-path))

  (customize-set-value 'file-cache-ignore-case t)

  ;; mini bufferでfile名補完中のFileCache起動キー
  (define-key minibuffer-local-completion-map
    (kbd "C-q C-i") 'file-cache-minibuffer-complete)

  (setq file-name-shadow-mode t) ;;ファイル名入力時に不用になった部分暗くする

  (require 'saveplace)  ;;以前編集していた位置を開く
  (setq-default save-place t)
  (auto-compression-mode t);; 圧縮ファイルを透過的に開く
  
  ;;----------------------------------------
  ;; elisp-install
  ;;----------------------------------------
  (require 'auto-install)
  (custom-set-variables
   '(auto-install-directory (concat my-elisp-directory "/new/"))
   '(install-elisp-repository-directory (concat my-elisp-directory "/rep/"))
   '(auto-install-update-emacswiki-package-name t))

  (require 'savehist) ;; mini buffer 入力履歴
  (setq savehist-mode 1)
  
  (setq kill-whole-line t) ;; C-kで行全体を削除
  (setq inhibit-startup-message t) ;;起動画面を表示しない
  (setq enable-recursive-minibuffers t) ;;前のcommandが終了してなくても、新しいcommandを実行可能にする。
  (global-auto-revert-mode t) ;;file が他から変更されたら、自動的に読み込む。
  (delete-selection-mode t) ;; マーク選択中の編集コマンドの挙動変更/範囲削除
  (customize-set-value 'indicate-empty-lines t) ;; バッファの最後に空行を追加
  ;; (setq default-indicate-empty-lines t)
  (customize-set-value 'next-line-add-newlines nil) ;; カーソル移動で行を作らない

  ;; 改行コード表示をわかりやすく
  (setq eol-mnemonic-dos "(CRLF)")
  (setq eol-mnemonic-mac "(CR)")
  (setq eol-mnemonic-unix "(LF)")
  (setq eol-mnemonic-undecided "(INVALID)")

  (setq indent-tabs-mode nil) ;; インデントにtab を使うか
  (setq tab-width 2)  ;; tab 幅設定
  ;; (setq default-tab-width 2 )
  (setq redisplay-dont-pause t)  ;; キーリピートにカーソルを追随させる
  (setq undo-outer-limit 10000);; undo の保存限界
  
  (random 1000000) ;; Seed the random-number generator
  
  ;;mini buffer での質問に yes/no を入力するのは面倒なのでSPC で yes とする。
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq message-log-max 100000) ;; messageバッファのログ数
  (setq use-dialog-box nil) ;; ダイアログはつかわない
  ;; (setq echo-keystrokes 0.15) ;;
  (setq echo-keystrokes 0.0) ;;

  ;;
  (defadvice save-buffers-kill-terminal (before my-save-buffers-kill-terminal activate)
  (when (process-list)
    (dolist (p (process-list))
      (set-process-query-on-exit-flag p nil))))

  (setq require-final-newline t) ; file の最後は 必ず newline で終わる様にする。
  (setq line-number-display-limit 10000)   ;;表示される最大行数を大きくする。

  ;;ホイールマウス
  (mouse-wheel-mode t)
  (setq mouse-wheel-follow-mouse t) ;;

  ;; scroll force 1line.
  (setq scroll-conservatively 35
        scroll-margin 0
        scroll-step 1)

  (setq completion-ignore-case t) ;; 補完時に大文字小文字を区別しない
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (set-scroll-bar-mode nil)

  (auto-image-file-mode)
  (setq-default line-spacing 0) ;; setting pixel between lines

  ;; 最近使った file を記憶させる。
  (require 'recentf)
  (recentf-mode 1)
  (custom-set-variables
   '(recentf-max-menu-items 500)
   '(recentf-max-saved-items 500)
   )

  ;;別のdirectoryにある同一名のfileを開いた時に、
  ;;numberingではなく、directorynameを表示して区別出来る様にする。
  (require 'uniquify)
  (custom-set-variables
   '(uniquify-buffer-name-style 'post-forward-angle-brackets)
   '(uniquify-ignore-buffers-re "*[^*]+*")
   '(uniquify-buffer-name-style 'forward)
   )

  ;;ファイル名がカーソルの下にある場合にfind-fileで開く
  (require 'ffap)

  ;;----------------------------------------
  ;; backup files.
  ;;----------------------------------------
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
  ;;beep音を消す
  (setq ring-bell-function '(lambda ()))
  (setq visible-bell t)

  (setq undo-outer-limit 1000000)

  ;;-------------------------------
  ;; マック用設定
  ;;-------------------------------
  (when (my-is-mac)
    ;; Swap Command-Key, Option-Key
    (setq ns-command-modifier (quote meta))
    (setq ns-alternate-modifier (quote super))
    (define-key global-map [ns-drag-file] 'ns-find-file)
    (setq ns-pop-up-frames nil) ;; 新規フレームなし。
    )

  ;;----------------------------------------
  ;; ウィンドウズ用設定
  ;;----------------------------------------
  (when (my-is-windows)
    (setq w32-pass-alt-to-system t) ;; ALTの入力をwindowsに通知する 最大化、移動用
    (setq w32-phantom-key-code t)
    (setq w32-use-full-screen-buffer t)
    (setq w32-hide-mouse-timeout 1)
    (setq w32-hide-mouse-on-key t)

    ;;cygwin風ディレクリ指定
    (require 'cygwin-mount)
    (cygwin-mount-activate)
    )
  )

(provide 'init-basic)
