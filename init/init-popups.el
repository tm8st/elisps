;;; init-popups.el --- popup menu setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, popup
;; creation time: Tue May  4 22:10:11 2010
;;; Commentary:

;;; Code:

(require 'popup)

(set-face-foreground 'popup-face "white")
(set-face-foreground 'popup-menu-selection-face "pink")
(set-face-background 'popup-face "gray30")
(set-face-background 'popup-menu-selection-face "gray30")

;;;-------------------------------
;;; mail template
;;;-------------------------------
(defun my-insert-mail-sentence-template () 
  (interactive)
  (insert
   (replace-regexp-in-string "\"" ""
    (replace-regexp-in-string "\\\\" ""
			      (prin1-to-string
			       (popup-menu* my-mail-sentence-template-list))))))

;;;-------------------------------
;;; 連想リスト用
;;;-------------------------------
(defun popup-menu-assoc (assoc-list) 
  (interactive)
  (car (cdr (assoc 
	     (popup-menu*
	      (my-popup-assoc-list-to-menu-item assoc-list))
	     assoc-list))))

(defun my-popup-assoc-list-to-menu-item (assoc-list)
  (let ((temp nil))
    (dolist (node assoc-list)
      (setq temp (cons (car node) temp)))
    (reverse temp)))

;;;-------------------------------
;;; ビルドコマンド
;;;-------------------------------
(defun my-jump-to-project-root () 
  (interactive)
  (dired
   (concat (shell-command-to-string "global -p") "/")))

(defun my-insert-project-root () 
  (interactive)
  (insert
   (concat (shell-command-to-string "global -p") "/")))

(defun my-build-command-template () 
  (interactive)
  (insert
   (popup-menu-assoc my-build-command-assoc-template-list)))

;;;-------------------------------
;;; ディレクリジャンプ
;;;-------------------------------
(defun my-directory-shirtcut ()
  (interactive)
  (dired
   (concat
    (popup-menu-assoc my-directory-name-assoc-template-list)
    )
   ))

(global-set-key (kbd "C-l C-m") 'my-insert-mail-sentence-template)
(global-set-key (kbd "C-l C-b") 'my-build-command-template)
(global-set-key (kbd "C-l C-v") 'my-directory-shirtcut)

(provide 'init-popups)
