;;; init-ruby.el --- ruby setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, ruby
;; creation time: Wed Apr 28 00:55:31 2010
;;; Commentary:

;;; Code:

(my-require 'ruby-mode)
;; (my-require 'inf-ruby)
;; (my-require 'ruby-block)
(my-require 'ruby-electric)
;;(autoload 'ruby-mode "ruby-mode"
;;  "Mode for editing ruby source files" t)
(setq auto-mode-alist
	  (append '(("\\.rb$" . ruby-mode))
			  auto-mode-alist))

;; (autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)

(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
									 interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
		  '(lambda () (inf-ruby-keys)))

(define-key ruby-mode-map (kbd "C-m") 'my-backward-word)


(defun my-run-ruby-buffer ()
  (interactive)
  ;; (start-process-shell-command "*ruby run*" nil (concat "ruby" (buffer-file-name (current-buffer)))))
  (shell-command
   (concat "ruby " (buffer-file-name (current-buffer)))))

;; ruby run.
(define-key ruby-mode-map (kbd "C-c C-c")
  `my-run-ruby-buffer)

;;;-------------------------------
;;; rsense
;;;-------------------------------
;; (add-to-list 'load-path (concat my-rsense-home "/etc"))
;; (my-require 'rsense)
;; (customize-set-value 'rsense-home my-rsense-home)

;; ;; $RSENSE_HOMEはRSenseをインストールしたディレクトリのフルパスに置き換えてください
;; ;; (setq rsense-home (expand-file-name "~/opt/rsense-0.2"))
;; ;; (setq rsense-home "~/opt/rsense-0.2")
;; ;; UNIX系システムでの例
;; ;; (setq rsense-home "/home/tomo/opt/rsense-0.2")
;; ;; あるいは
;; ;; (setq rsense-home (expand-file-name "~/opt/rsense-0.2"))
;; ;; Windowsでの例
;; ;; (setq rsense-home "C:\\rsense-0.2")

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-o") 'ac-complete-rsense)
;; 	    (local-set-key (kbd "C-c C-o") 'rsense-type-help)
;; 	    )
;; 	  )

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;; ;;;-------------------------------
;; ;;; reference
;; ;;;-------------------------------
;; (defun refe2x (kw)
;;   (interactive "sReFe2x: ")
;;   (let ((coding-system-for-read 'euc-japan))
;;     (with-current-buffer (get-buffer-create (concat "*refe2x:" kw "*"))
;;       (when (zerop (buffer-size))
;;         (call-process "refe2x" nil t t kw)
;;         (diff-mode))
;;       (setq minibuffer-scroll-window (get-buffer-window (current-buffer) t))
;;       (goto-char (point-min))
;;       (display-buffer (current-buffer)))))

;;;-------------------------------
;;; refe
;;;-------------------------------
(defun my-refe-1-9-1 ()
  "リファレンス検索"
  (interactive)
  (if mark-active
      (let* ((start (mark)) (end (point)) (input (buffer-substring start end)))
	(shell-command (concat "refe-1_9_1 " input)))
    (progn
      (let* ((input (read-string "Search String:")))
      (shell-command (concat "refe-1_9_1 " input))))))
(defun my-refe-1-8 ()
  "リファレンス検索"
  (interactive)
  (if mark-active
      (let* ((start (mark)) (end (point)) (input (buffer-substring start end)))
	(shell-command (concat "refe-1_8_7 " input)))
    (progn
      (let* ((input (read-string "Search String:")))
      (shell-command (concat "refe-1_8_7 " input))))))

;; ruby refe.
(define-key ruby-mode-map (kbd "C-l C-r")
  `my-refe-1-8)

(provide 'init-ruby)
