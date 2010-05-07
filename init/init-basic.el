;;; init-basic.el --- basic customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

;;;----------------------------------------
;;; 文字コード
;;;----------------------------------------
(set-language-environment 'Japanese)

;; (set-default-coding-systems 'euc-jp)
;; (set-buffer-file-coding-system 'euc-jp)
;; (set-terminal-coding-system 'euc-jp)
;; (prefer-coding-system 'euc-jp)
;; (setq file-name-coding-system 'euc-jp)
;; (set-keyboard-coding-system 'euc-jp)
;; (set-clipboard-coding-system 'euc-jp)

(prefer-coding-system 'utf-8)

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
  
  ;; (setq url-proxy-services '(("http" . "localhost:8339")))
  ;; (auto-install-compatibility-setup)
  
  ;; (let ((buffer (url-retrieve-synchronously
  ;;                "http://tromey.com/elpa/package-install.el")))
  ;;   (save-excursion
  ;;     (set-buffer buffer)
  ;;     (goto-char (point-min))
  ;;     (re-search-forward "^$" nil 'move)
  ;;     (eval-region (point) (point-max))
  ;;     (kill-buffer (current-buffer))))
  
  )

;;;-------------------------------
;;; GUI Setting
;;;-------------------------------
(when use-gui-setting

  ;;-------------------------------
  ;; Font setting
  ;;-------------------------------
  ;; ベースのフォントサイズ OSによって微妙に変更
  (defvar my-font-size-base 100)
  (cond
   ((my-is-windows)	(setq my-font-size-base 120))
   ((my-is-mac)		(setq my-font-size-base 150))
   )

  (when use-font-setting
    (set-face-attribute 'default nil
			;; :family "VL Pゴシック"
                        :height my-font-size-base)

    ;; (set-face-attribute 'default nil
    ;; 			:family "VL ゴシック"
    ;; 			:height my-font-size-base)

    ;; (setq face-font-rescale-alist
    ;; 	  '((".*profont-medium.*" . (* my-font-size-base 1.5))
    ;; 	    (".*profont-bold.*" . (* my-font-size-base 1.8))
    ;; 	    ("-cdac$" . (* my-font-size-base 1.8))))
    )

  ;;-------------------------------
  ;; frame setting
  ;;-------------------------------
  ;; default frame setting
  (defvar my-frame-setting-list
    (list
     '(background-color . "gray4") ;; 背景色
     '(foreground-color . "gray80") ;; 文字色
     '(cursor-color . "green1") ;; カーソル色
     '(cursor-type . box) ;; カーソル形状
     '(cursor-height . 4) ;; カーソルの高さ
     '(mouse-color . "white") ;; マウスカーソル色
     '(border-color . "white") ;; 縁の色
     '(vertical-scroll-bars . 'right) ;; スクロールバー
     '(width . 160) ;; 横幅(桁数)
     '(height . 82)	;; 高さ(行数)
     '(left . 0) ;; 左上隅 x 座標
     '(top . 0) ;; 左上隅 y 座標
     '(alpha . 100) ;; 透明度設定
     )
    )

  ;; (add-to-list 'default-frame-alist my-frame-setting-list)
  ;; (add-to-list 'initial-frame-alist my-frame-setting-list)

  (setq default-frame-alist
        (append
         my-frame-setting-list
         default-frame-alist))

  (setq initial-frame-alist
        (append
         my-frame-setting-list
         initial-frame-alist))

  ;;----------------------------------------
  ;; color setting.
  ;;@memo M-x list-colors-display
  ;;----------------------------------------

  (setq font-lock-support-mode 'jit-lock-mode)
  (global-font-lock-mode 1)			  ;;色を付ける
  (setq font-lock-maximum-decoration t)
  
  (set-face-foreground 'default "gray80")
  (set-face-background 'default "gray4")

  (set-face-background 'highlight "gray45")
  ;; (set-face-background 'highlight "Gray40")

  (set-face-foreground 'font-lock-comment-face "green")
  ;;  (set-face-foreground 'font-lock-comment-face "cornsilk4")
  (set-face-foreground 'font-lock-string-face "sandy brown")
  (set-face-foreground 'font-lock-keyword-face "pink")
  (set-face-foreground 'font-lock-function-name-face "pink")
  (set-face-foreground 'font-lock-variable-name-face "pink")
  (set-face-foreground 'font-lock-type-face		"pink")
  (set-face-foreground 'font-lock-constant-face	"pink")
  (set-face-foreground 'font-lock-warning-face	"pink")
  (set-face-bold-p 'font-lock-warning-face nil)
  (set-face-background 'region "dark green")  ;;リージョン色
  ;; (set-face-background 'region "gray30")	 ;;リージョン色
  (set-face-background 'isearch "gray30")	;;isearchの色設定
  (set-face-foreground 'isearch "green")

  ;; chg [Sat May 09 03:51:44 2009]
  ;; (set-face-foreground 'font-lock-comment-face "wheat4")
  ;; (set-face-foreground 'font-lock-comment-face "gray48")
  ;; (set-face-foreground 'font-lock-comment-face "wheat4")
  ;; (set-face-foreground 'font-lock-comment-face "CadetBlue4")
  ;; (set-face-foreground 'font-lock-string-face "gray60")

  ;; modeline
  (set-face-background 'modeline "gray16")
  (set-face-foreground 'modeline "gray70")

  ;;タイトルバーにファイル名を表示するタイトルフォーマット設定
  (setq frame-title-format
        (format "emacs %s : %%f %s"
                (system-name)
                (emacs-version)))

  (display-time) ;;モードラインに時間を表示する
  (which-function-mode 1)  ;;現在の関数名をモードラインに表示
  (column-number-mode t) ;;カーソルの位置が何文字目かを表示する
  (line-number-mode t) ;;カーソルの位置が何行目かを表示する
  
  ;;-------------------------------
  ;; 日本語入力のON/OFFでカーソルの色を変える 
  ;;-------------------------------
  (when (my-is-windows)
    (add-hook 'mw32-ime-on-hook
	      (function (lambda () (set-cursor-color "Pink"))))
    (add-hook 'mw32-ime-off-hook
	      (function (lambda () (set-cursor-color "White"))))
    (setq-default mw32-ime-mode-line-state-indicator "[--]")
    )

  ;;-------------------------------
  ;; 現在行の強調 
  ;;-------------------------------
  (require 'hl-line)
  
  (defface my-hl-line-face
    '((((class color)
	(background dark))
       (:background "gray10"))
      (((class color)
	(background light))
       (:background "ForestGreen"))
      (t
       ()))
    "*Face used by hl-line."
    :group 'my)

  ;; (set-face-foreground 'my-hl-line-face "white")
  ;; (set-face-background 'my-hl-line-face "gray20")
  (customize-set-value 'hl-line-face 'highlight)

  ;; (setq hl-line-face 'underline) ; 下線
  (global-hl-line-mode t)
  
  ;;-------------------------------
  ;; 対応する括弧を強調表示
  ;;-------------------------------
  (require 'paren)								;;対応する括弧を強調表示 & 対応しない場合は 警告する。
  (show-paren-mode t)							;;対応する括弧を表示する
  (set-face-background 'show-paren-match "blue")
  (custom-set-variables
   '(show-paren-ring-bell-on-mismatch t);;対応しない括弧を探す。
   '(show-paren-style 'mixed)				;;対応する括弧が画面内になければ、括弧内を強調表示。
   )
  ;; より広範囲に色づけ
  (require 'highlight-parentheses)
  (highlight-parentheses-mode)
  (custom-set-variables
   '(hl-paren-colors '("orange" "blue" "yellow" "red"))
   '(hl-paren-background-colors nil)
   '(hl-paren-background-colors '("tomato4" "tomato3" "tomato2" "tomato1"))
   )

  ;;-------------------------------
  ;; 折り返し設定
  ;;-------------------------------
  ;; (setq truncate-lines t)
  ;; (setq truncate-partial-width-windows t) ;; 縦分割画面で、長い行をwrapさせる。aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
  (add-hook 'find-file-hook
	    '(lambda ()
	       (setq truncate-lines t)
	       (setq truncate-partial-width-windows t)
	       )
	    )

  (set-scroll-bar-mode 'right) ;; スクロールバーを右側に表示する
  (blink-cursor-mode 0) ;;カーソルの点滅を止める
  (customize-set-value 'next-line-add-newlines nil)
  (tool-bar-mode nil)
  (menu-bar-mode nil)
  (tooltip-mode -1) 
  (tool-bar-mode -1)
  
  ;;-------------------------------------
  ;; タブや全角スペースを表示
  ;; mac だと通常のfont-lockと競合してしまう
  ;;-------------------------------------
  (when (my-is-windows)
    (defface my-face-full-space
      '((t :background "gray60"))
      "full space face"
      :group 'my)

    (defface company-tooltip
      '((t :background "yellow"
           :foreground "black"))
      "*Face used for the tool tip."
      :group 'company)

    (defvar my-face-full-space 'my-face-full-space
      "full space face.")

    (defadvice font-lock-mode (before my-font-lock-mode ())
      (font-lock-add-keywords
       major-mode
       '(
         ;; ("\t" 0 my-face-tab append)
         ("　" 0 my-face-full-space append)
         )))
    (ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
    (ad-activate 'font-lock-mode)
    )

  ;;-------------------------------
  ;; コンテキストに合わせてカーソル変更
  ;; mac だとやけに遅い。
  ;;-------------------------------
  (when (my-is-windows)
    (require 'cursor-chg)
    (custom-set-variables
     '(curchg-change-cursor-on-overwrite/read-only-flag t)
     '(curchg-change-cursor-on-input-method-flag t)
     '(cursor-mode t) ; On for overwrite/read-only/input mode
     '(curchg-idle-cursor-type 'hbar)
     '(curchg-overwrite/read-only-cursor-type 'hollow)
     )

    (toggle-cursor-type-when-idle t) ; On when idle
    ;; (setq curchg-idle-cursor-type 'hbar)
    )
  )

(provide 'init-basic)
