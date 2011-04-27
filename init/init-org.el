;;; init-org.el --- org mode setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, org
;; creation time: Wed Apr 28 00:53:29 2010
;;; Commentary:

;;; Code:

(require 'org)
(require 'org-install)
(require 'org-agenda)
(require 'org-mobile)

(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(setq org-log-done 'time) ;; DONEの時刻を記録
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate) ;; remember も使う
(add-hook 'org-mode-hook 'turn-on-font-lock) ;; org-modeでの強調表示を可能にする
;; (setq org-hide-leading-stars t) ;; 見出しの余分な*を消す
(setq org-hide-leading-stars nil) ;; 見出しの余分な*を消さない

(setq org-agenda-include-all-todo t)
(setq org-tags-column -100)
(setq org-fast-tag-selection-single-key t)

(customize-set-variable 'org-agenda-include-diary t)

(if my-use-dropbox
		(setq org-directory (concat my-dropbox-directory "Org/"))
	(setq org-directory "~/.emacs.d/org/"))

(setq org-default-notes-file (concat org-directory "main.org"))
(setq org-agenda-files (list org-directory))

;; TODOの状態遷移
(setq org-todo-keywords
      '((sequence "TASK(t)" "WAIT(w)" "DONE(d)" "SOMEDAY(s)")))

;; DONEの時刻を記録
(setq org-log-done 'time)

(setq org-remember-templates
      '(("TASK" ?t "** TASK \n  %i\n  %a\n  %U\n" nil "INBOX")
        ("IDEA" ?i "** SOMEDAY \n  %i\n  %a\n  %U\n" nil "IDEAS")
				("MEMO" ?m "** \n  %i\n  %a\n  %U\n" nil "NOTES")
        ))

(setq org-mobile-inbox-for-pull (concat org-directory "pulled.org"))
;; Dropbox直下のMobileOrgフォルダへのパスを設定
(setq org-mobile-directory (concat my-dropbox-directory "MobileOrg"))

;; 標準の祝日を利用する
(customize-set-variable 'org-calendar-holidays t)

;; キーバインドの設定
(define-key global-map (kbd "C-l C-o C-r") 'org-remember)

;; index fileを開く
(defun my-org-open-index ()
	(interactive)
	(find-file org-default-notes-file))

(define-key global-map (kbd "C-l C-o C-o") 'my-org-open-index)
(define-key global-map (kbd "C-l C-o C-l") 'org-store-link)

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

;; timer start, stop.
(define-key org-mode-map (kbd "C-c C-i") 'org-clock-in)
(define-key org-mode-map (kbd "C-c C-o") 'org-clock-out)

(define-key org-agenda-mode-map (kbd "C-i") 'org-agenda-clock-in)
(define-key org-agenda-mode-map (kbd "C-o") 'org-agenda-clock-out)

(provide 'init-org)
