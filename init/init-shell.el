;;; init-shell.el --- shell setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, shell
;; creation time: Wed Apr 28 00:56:26 2010
;;; Commentary:

;;; Code:

;;;----------------------------------------
;;; shell setting.
;;;----------------------------------------
(when (my-is-windows)
  (setq shell-file-name "bash.exe")
  (setq explicit-shell-file-name "bash.exe")
  ;; (modify-coding-system-alist 'process ".*bash\.exe" '(undecided-dos . euc-japan))
  ;; (modify-coding-system-alist 'process ".*sh\.exe" '(undecided-dos . euc-japan))
  )
(when (my-is-mac)
  (setq shell-file-name "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash")
  ;; (setq shell-file-name "/bin/bash")
  ;; (setq explicit-shell-file-name "/bin/bash")
  )

(setq shell-command-switch "-lc") ;; デバッグ用

;; (setq shell-command-switch "--rcfile $HOME/.bashrc -c ")
;; (setq shell-command-switch " -x -c ")
;; (setq shell-command-switch "--rcfile -c")

;; (setq shell-command-switch "-cx") ;; 設定ファイルを読むためにログインにする？
;; (setq shell-command-switch "-lc") ;; デバッグ用
;; (setq shell-command-switch "-xc") ;; 設定ファイルを読むためにログインにする？ デバッグ用

;; shell-mode 色設定
(autoload 'ansi-color-for-comint-mode-on "ansi-color" "Set `ansi-color-for-comint-mode' to t." t)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;; shell-mode コマンド履歴
(require 'shell)
(define-key shell-mode-map (kbd "C-S-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-S-n") 'comint-next-input)
(define-key shell-mode-map (kbd "C-c C-j") 'compilation-mode)

(setq comint-scroll-show-maximum-output t) 	;; できるだけ出力内容が見えるようにスクール
(setq comint-scroll-to-bottom-on-input t) 		;; テキスト入力時に自動でスクロール
(setq comint-input-ignoredups t)				;; 同じコマンド履歴は残さない

;;tload -d グラフの更新間隔秒 -s 縦軸の目盛り数 [ tty]

;;;-------------------------------
;;; multi-shell
;;;-------------------------------
(require 'comint)
(require 'multi-shell)
(setq multi-shell-use-ansi-color t)
;; (setq multi-shell-bottom-window-height 12)
;; (setq multi-shell-buffer-name "*multi-shell*")

;; 
(global-set-key (kbd "C-l C-s C-n") 'multi-shell-new)
(global-set-key (kbd "C-l C-s C-d") 'multi-shell-current-directory) ;; 現在ディレクトリでshell開始
(global-set-key (kbd "C-l C-s C-f") 'multi-shell-next)
(global-set-key (kbd "C-l C-s C-b") 'multi-shell-prev)

;;;-------------------------------
;;; shell-pop 
;;;-------------------------------
(require 'shell-pop)
;; (shell-pop-set-internal-mode "ansi-term")
;; (shell-pop-set-internal-mode-shell "/bin/zsh")
;; (defvar ansi-term-after-hook nil)
;; (add-hook 'ansi-term-after-hook
;;           '(lambda ()
;;              (define-key term-raw-map (kbd "C-l C-s C-p") 'shell-pop)))
;; (defadvice ansi-term (after ansi-term-after-advice (org))
;;   "run hook as after advice"
;;   (run-hooks 'ansi-term-after-hook))
;; (ad-activate 'ansi-term)

(global-set-key (kbd "C-l C-s C-p") 'shell-pop)

(provide 'init-shell)
