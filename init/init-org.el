;;; init-org.el --- org mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, org
;; creation time: Wed Apr 28 00:53:29 2010
;;; Commentary:

;;; Code:

(require 'org)
(require 'org-install)
(require 'org-agenda)
(require 'org-mobile)
(require 'org-habit)

(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(setq org-log-done 'time) ;; DONEの時刻を記録
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate) ;; remember も使う

(add-hook 'org-mode-hook
   '(lambda ()
      (turn-on-font-lock)
      (setq indent-tabs-mode nil)
      (setq tab-width 2)
      (setq default-tab-width 2)
      ))

;; (setq org-hide-leading-stars t) ;; 見出しの余分な*を消す
(setq org-hide-leading-stars nil) ;; 見出しの余分な*を消さない

(setq org-agenda-include-all-todo t)
(setq org-tags-column -100)
(setq org-fast-tag-selection-single-key t)

(require 'auto-complete)
(add-to-list 'ac-modes 'org-mode)

(customize-set-variable 'org-agenda-include-diary t)

(require 'simple)
(define-key org-mode-map (kbd "C-j") 'newline)
(define-key org-mode-map (kbd "C-c C-j") 'org-return-indent)

(if my-use-dropbox
		(setq org-directory (concat my-dropbox-directory "Org/"))
	(setq org-directory "~/.emacs.d/org/"))

(setq org-default-notes-file (concat org-directory "main.org"))
(setq org-agenda-files (list org-directory))

;; TODOの状態遷移
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)" "|" "DONE(d)" "SOMEDAY(s)")))

;; DONEの時刻を記録
(setq org-log-done 'time)

(require 'org-remember)
(setq org-remember-templates
      '(
				("TASK" ?t "** TASK \n  %i\n  %a\n  %U\n" nil "INBOX")
				("IDEA" ?i "** SOMEDAY \n  %i\n  %a\n  %U\n" nil "IDEAS")
				("MEMO" ?m "** \n  %i\n  %a\n  %U\n" nil "NOTES")
				("LIFE-TASK" ?l "** TASK     \n  %i\n  %a\n  %U\n" nil "LIFE")
				("HABIT" ?h "** \n:PROPERTIES:\n:LOGGING: DONE(!) logrepeat\n:END:\n%i\n  %a\n  %U\n" nil "HABIT")
        ))

(setq org-mobile-inbox-for-pull (concat org-directory "pulled.org"))
;; Dropbox直下のMobileOrgフォルダへのパスを設定
(setq org-mobile-directory (concat my-dropbox-directory "MobileOrg"))
(setq org-mobile-force-id-on-agenda-items nil)

;; 標準の祝日を利用する
(customize-set-variable 'org-calendar-holidays t)

;; キーバインドの設定
(define-key global-map (kbd "C-l C-o C-r") 'org-remember)

;; index fileを開く
(defun my-org-open-index ()
	(interactive)
	(find-file org-default-notes-file))

(define-key global-map (kbd "C-l C-o C-o") 'my-org-open-index)
(define-key global-map (kbd "C-l C-o C-s") 'org-store-link)

;; (defun my-org-mobile-sync ()
;; 	(org-mobile-pull)
;; 	(org-mobile-push))

(define-key global-map (kbd "C-l C-o C-p") 'org-mobile-push)
(define-key global-map (kbd "C-l C-o C-@") 'org-mobile-pull)

(define-key org-mode-map (kbd "C-TAB") 'org-force-cycle-archived)
(define-key org-mode-map [(meta left)]  'org-metaleft)
(define-key org-mode-map [(meta right)] 'org-metaright)
(define-key org-mode-map [(meta up)]    'org-metaup)
(define-key org-mode-map [(meta down)]  'org-metadown)

(define-key org-mode-map (kbd "M-N") 'org-metadown)
(define-key org-mode-map (kbd "M-P") 'org-metaup)
(define-key org-mode-map (kbd "M-n") 'org-forward-same-level)
(define-key org-mode-map (kbd "M-p") 'org-backward-same-level)
(define-key org-mode-map (kbd "C-m") 'my-backward-word)
(define-key org-mode-map (kbd "C-M-m") 'bm-toggle)
(define-key org-mode-map (kbd "C-M-j") 'org-insert-heading)

;;;-------------------------------
;;; org-agenda setting.
;;;-------------------------------
(require 'org-agenda)

(add-hook 'org-agenda-mode-hook
   '(lambda ()
    (hl-line-mode 1)
    (setq hl-line-face 'underline)))

(define-key global-map (kbd "C-l C-o C-a") 'org-agenda)
(define-key global-map (kbd "C-l C-o C-l") 'org-agenda-list)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*Org Agenda*" :height 0.5))

(defun my-org-agenda-open-buffer ()
	(interactive)
	(display-buffer "*Org Agenda*"))

(define-key global-map (kbd "C-l C-o C-z") 'my-org-agenda-open-buffer)

(defun my-org-clock-in ()
	(interactive)
	(tm8st-growl-timer "Org" "Org" nil (* 60 1000))
	(org-clock-in))

(defun my-org-clock-out ()
	(interactive)
	(tm8st-growl-notify-delete-sticky "DONE!!" "Org")
	(org-clock-out))

;; timer start, stop.
(define-key org-mode-map (kbd "C-c C-i") 'my-org-clock-in)
(define-key org-mode-map (kbd "C-c C-o") 'my-org-clock-out)

(define-key org-agenda-mode-map (kbd "C-i") 'org-agenda-clock-in)
(define-key org-agenda-mode-map (kbd "C-o") 'org-agenda-clock-out)

(defun my-add-separate-char-on-newline ()
	"ざっくりとレイアウト調整。"
	(interactive)
	(let ((s (point))
				(e (mark)))
		(replace-string "," ",\n" nil s e)
		(replace-string "\. " ".\n" nil s e)
		(replace-string "!" "!\n" nil s e)
		))

(define-key org-mode-map (kbd "C-c C-k") 'my-add-separate-char-on-newline)

;; 空行の削除
(setq org-cycle-separator-lines 0)
(setq org-blank-before-new-entry (quote ((heading)
                                         (plain-list-item))))

(setq org-show-following-heading t)
(setq org-show-hierarchy-above t)
(setq org-show-siblings nil)

;; checklistのタスク完了時のリセット。
;; :RESET_CHECK_BOXES: t
(require 'org-checklist)
(setq org-reverse-note-order nil)
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
; Which tag is used to mark headings to be encrypted
(setq org-tags-exclude-from-inheritance (quote ("CRYPT")))
(customize-set-value 'org-crypt-tag-matcher "CRYPT")
; GPG key to use for encryption
(setq org-crypt-key "F0B66B40")

;; おちる。。。
;; (defun my-org-hide-other-subtree ()
;;   (interactive)
;;   (org-narrow-to-subtree)
;;   ;; (org-show-todo-tree nil)
;;   )
;; (global-set-key (kbd "C-l C-o C-v") 'my-org-hide-other-subtree)

(provide 'init-org)
