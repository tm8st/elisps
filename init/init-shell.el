;;; init-shell.el --- shell setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, shell
;; creation time: Wed Apr 28 00:56:26 2010
;;; Commentary:

;;; Code:

(my-require 'init-keybindings)
(my-require 'shell)
(setq shell-command-switch "-lc") ;; デバッグ用
(my-require 'shell-history)

;; (setq shell-command-switch "--rcfile $HOME/.bashrc -c ")
;; (setq shell-command-switch " -x -c ")
;; (setq shell-command-switch "--rcfile -c")

;; (setq shell-command-switch "-cx") ;; 設定ファイルを読むためにログインにする？
;; (setq shell-command-switch "-lc") ;; デバッグ用
;; (setq shell-command-switch "-xc") ;; 設定ファイルを読むためにログインにする？ デバッグ用

;;; shell-mode コマンド履歴
(my-require 'comint)
(define-key shell-mode-map (kbd "C-S-p") 'comint-previous-input)
(define-key shell-mode-map (kbd "C-S-n") 'comint-next-input)
(define-key shell-mode-map (kbd "C-m") my-backward-word-command)
(define-key shell-mode-map (kbd "C-j") 'comint-send-input)
(define-key shell-mode-map (kbd "C-i") 'comint-dynamic-complete)
(define-key shell-mode-map (kbd "C-c C-a") 'comint-show-output)
(define-key shell-mode-map (kbd "C-c C-e") 'comint-show-maximum-output)
(define-key shell-mode-map (kbd "C-c C-n") 'comint-next-prompt)
(define-key shell-mode-map (kbd "C-c C-p") 'comint-previous-prompt)
(define-key shell-mode-map (kbd "C-c C-/") 'comint-quit-subjob)

;; toggle shell <-> compilation
(define-key shell-mode-map (kbd "C-c C-j") 'compilation-mode)
(define-key compilation-mode-map (kbd "C-c C-j") 'shell-mode)

(setq comint-scroll-show-maximum-output t) 	;; できるだけ出力内容が見えるようにスクール
(setq comint-scroll-to-bottom-on-input t) 		;; テキスト入力時に自動でスクロール
(setq comint-input-ignoredups t)				;; 同じコマンド履歴は残さない

;;;-------------------------------
;;; multi-shell
;;;-------------------------------
(my-require 'multi-shell)
(setq multi-shell-use-ansi-color t)
(setq multi-shell-revert-window-after-complete nil)

;; (setq multi-shell-bottom-window-height 12)
;; (setq multi-shell-buffer-name "*multi-shell*")

;; key bind
(global-set-key (kbd "C-l C-s C-n") 'multi-shell-new)
(global-set-key (kbd "C-l C-s C-d") 'multi-shell-current-directory)
(global-set-key (kbd "C-l C-s C-f") 'multi-shell-next)
(global-set-key (kbd "C-l C-s C-b") 'multi-shell-prev)

;;;-------------------------------
;;; shell-pop 
;;;-------------------------------
(my-require 'shell-pop)
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

;;;----------------------------------------
;;; shell setting.
;;;----------------------------------------
(when (my-is-windows)
  (setq shell-file-name "bash.exe")
  (setq explicit-shell-file-name "bash.exe")
  (setq multi-shell-command "bash.exe")
  ;; (modify-coding-system-alist 'process ".*bash\.exe" '(undecided-dos . euc-japan))
  ;; (modify-coding-system-alist 'process ".*sh\.exe" '(undecided-dos . euc-japan))
  )
(when (my-is-mac)
  (setq shell-file-name "/bin/bash")
  (setq explicit-shell-file-name "/bin/bash")
  (setq multi-shell-command "/bin/bash")
  ;; (setq shell-file-name "/bin/bash")
  ;; (setq explicit-shell-file-name "/bin/bash")
  ;; (setq multi-shell-command "/usr/local/bin/zsh")
  )

(when (my-require 'tm8st-growl)
	(my-require 'deferred)
	(defun my-shell-command-growl-notify (&optional cmd args)
		"shell-commandを実行し、終了したらgrowlでお知らせ。"
		(interactive)
		(lexical-let* ((cmd (if cmd cmd (read-string "shell command:")))
									 (args (if args args (read-string "command args:")))
									 (clsr (concat cmd ":" args)))
			(deferred:$
				(deferred:process-shell cmd args)
				(deferred:nextc it
					(lambda (x)
						(tm8st-growl-notify (concat "\"" "Finish! " clsr "\n" x "\""))
						(when (stringp x)
							(message x))))
				(deferred:error it
					(lambda (err)
						(tm8st-growl-notify (concat "\"" "Error! " clsr "\"")))))))

(global-set-key (kbd "C-l C-s C-t") 'my-shell-command-growl-notify))

;; shell-mode 色設定
;; (my-require 'comint)
;; (customize-set-value 'ansi-color-for-comint-mode nil)
(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)

(setq ansi-color-names-vector
      ["#000000"           ; black
       "#ff6565"           ; red
       "#93d44f"           ; green
       "#eab93d"           ; yellow
       "#204a87"           ; blue
       "#ce5c00"           ; magenta
       "#89b6e2"           ; cyan
       "#ffffff"]          ; white
      )
(ansi-color-for-comint-mode-on)

(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-show-maximum-output t)

;; (my-require 'multi-term)

(provide 'init-shell)
