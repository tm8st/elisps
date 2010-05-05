;;; init-org.el --- org mode setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, org
;; creation time: Wed Apr 28 00:53:29 2010
;;; Commentary:

;;; Code:

(require 'org)
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(setq org-log-done '(state))
(customize-set-variable 'org-agenda-include-diary t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; remember も使う
(org-remember-insinuate)

;; ファイル、ディレクトリ設定
(setq org-directory "~/.emacs.d/org/")
(setq org-default-notes-file (concat org-directory "gtd.org"))
(setq org-agenda-files (list org-directory))


;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook
	  '(lambda ()
	     (hl-line-mode 1)
	     (setq hl-line-face 'underline)
	     ))

;; 標準の祝日を利用しない
(customize-set-variable 'org-calendar-holidays nil)

;; TODOの状態遷移
(setq org-todo-keywords '("TODO" "Wait" "DONE")
      org-todo-interpretation 'sequence)

;; next actionのない項目の検索時の条件設定
(customize-set-variable 'org-stuck-projects
      '("+LEVEL=2" ("TODO" "Wait") ("SOMEDAY" "MEMO" "SCHEDULE")))

;; 項目設定
(customize-set-variable 'org-remember-templates
      '(
	("Work"  ?w "** TODO %? :WORK:\n   %i\n   %a\n   %t" nil "Tasks")
        ("TODO"  ?t "** TODO %? :ETC:\n   %i\n   %a\n   %t" nil "Tasks")
        ("Idea"  ?i "** IDEA %? :IDEA:\n %i\n %a\n %t" nil "Ideas")
        ("Anime" ?h "** MEMO %? :ANIME:\n   %i\n   %a\n   %t" nil "Anime")
        ("MEMO" ?m "** MEMO %? \n   %i\n   %a\n   %t" nil "Memo")
        ("Emacs MEMO" ?e "** MEMO %? :EMACS:MEMO:\n   %i\n   %a\n   %t" nil "Emacs")
	("Ruby MEMO"  ?r "** MEMO %? :Ruby:MEMO:\n   %i\n   %a\n   %t" nil "Ruby")
        ))

;; auto-isearchを使うので設定しない
;; (define-key org-goto-map "C-p" `outline-previous-visible-heading)
;; (define-key org-goto-map "C-n" `outline-next-visible-heading)

(defun my-open-gtd ()
  "GTDファイルを開く"
  (interactive)
  (find-file org-default-notes-file))

(define-key global-map (kbd "C-l C-;") 'org-remember)
(define-key global-map (kbd "C-l C-o C-a") 'org-agenda)
(define-key global-map (kbd "C-l C-o C-s") 'org-store-link)
(define-key global-map (kbd "C-l C-o C-o") 'my-open-gtd)
(define-key org-mode-map (kbd "C-l C-o C-t") 'org-todo)

(provide 'init-org)
