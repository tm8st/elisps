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
(setq org-log-done 'time) ;; DONEの時刻を記録
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(org-remember-insinuate) ;; remember も使う
(customize-set-variable 'org-agenda-include-diary t)
(setq org-directory "~/.emacs.d/org/")
(setq org-default-notes-file (concat org-directory "index.org"))
(setq org-agenda-files (list org-directory))
(setq org-remember-templates
      '(("TODO" ?t "** TODO %?\n   %i\n   %a\n   %t" nil "Inbox")
        ("Bug" ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
        ("Idea" ?i "** %?\n   %i\n   %a\n   %t" nil "New Ideas")
        ))

;; ファイル、ディレクトリ設定
;; MobileOrgで新しく作ったノートを保存するファイルの名前を設定
(setq org-mobile-inbox-for-pull "~/.emacs.d/org/pulled.org")
;; Dropbox直下のMobileOrgフォルダへのパスを設定
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; アジェンダ表示で下線を用いる
(add-hook 'org-agenda-mode-hook
   '(lambda ()
    (hl-line-mode 1)
    (setq hl-line-face 'underline)
    ))

;; 標準の祝日を利用しない
(customize-set-variable 'org-calendar-holidays nil)

;; TODOの状態遷移
;; (setq org-todo-keywords '("TODO" "Wait" "DONE")
;;       org-todo-interpretation 'sequence)

;; ;; next actionのない項目の検索時の条件設定
;; (customize-set-variable 'org-stuck-projects
;;       '("+LEVEL=2" ("TODO" "Wait") ("SOMEDAY" "MEMO" "SCHEDULE")))

;; ;; 項目設定
;; (customize-set-variable 'org-remember-templates
;;       '(
;; 	("Work"  ?w "** TODO %? :WORK:\n   %i\n   %a\n   %t" nil "Tasks")
;;         ("TODO"  ?t "** TODO %? :ETC:\n   %i\n   %a\n   %t" nil "Tasks")
;;         ("Idea"  ?i "** IDEA %? :IDEA:\n %i\n %a\n %t" nil "Ideas")
;;         ("Anime" ?h "** MEMO %? :ANIME:\n   %i\n   %a\n   %t" nil "Anime")
;;         ("MEMO" ?m "** MEMO %? \n   %i\n   %a\n   %t" nil "Memo")
;;         ("Emacs MEMO" ?e "** MEMO %? :EMACS:MEMO:\n   %i\n   %a\n   %t" nil "Emacs")
;; 	("Ruby MEMO"  ?r "** MEMO %? :Ruby:MEMO:\n   %i\n   %a\n   %t" nil "Ruby")
;;         ))

;; ;; auto-isearchを使うので設定しない
;; ;; (define-key org-goto-map "C-p" `outline-previous-visible-heading)
;; ;; (define-key org-goto-map "C-n" `outline-next-visible-heading)

;; (defun my-open-gtd ()
;;   "GTDファイルを開く"
;;   (interactive)
;;   (find-file org-default-notes-file))

;; (define-key global-map (kbd "C-l C-;") 'org-remember)
;; (define-key global-map (kbd "C-l C-o C-a") 'org-agenda)
;; (define-key global-map (kbd "C-l C-o C-s") 'org-store-link)
;; (define-key global-map (kbd "C-l C-o C-o") 'my-open-gtd)
;; (define-key org-mode-map (kbd "C-l C-o C-t") 'org-todo)

;; ;;;-------------------------------
;; ;;; rss reader
;; ;;;-------------------------------
;; (defun org-feed-parse-rdf-feed (buffer)
;;   "Parse BUFFER for RDF feed entries.
;; Returns a list of entries, with each entry a property list,
;; containing the properties `:guid' and `:item-full-text'."
;;   (let (entries beg end item guid entry)
;;     (with-current-buffer buffer
;;       (widen)
;;       (goto-char (point-min))
;;       (while (re-search-forward "<item[> ]" nil t)
;; 	(setq beg (point)
;; 	      end (and (re-search-forward "</item>" nil t)
;; 		       (match-beginning 0)))
;; 	(setq item (buffer-substring beg end)
;; 	      guid (if (string-match "<link¥¥>.*?>¥¥(.*?¥¥)</link>" item)
;; 		       (org-match-string-no-properties 1 item)))
;; 	(setq entry (list :guid guid :item-full-text item))
;; 	(push entry entries)
;; 	(widen)
;; 	(goto-char end))
;;       (nreverse entries))))

;; ; (setq org-feed-retrieve-method 'wget)
;; (setq org-feed-retrieve-method 'curl)

;; (setq org-feed-default-template "¥n* %h¥n  - %U¥n  - %a  - %description")

;; (setq org-feed-alist nil)

;; (add-to-list 'org-feed-alist
;;   '("hatena" "http://feeds.feedburner.com/hatena/b/hotentry"
;;     "~/org/rdf.org" "はてな"
;;     :parse-feed org-feed-parse-rdf-feed))
;; (add-to-list 'org-feed-alist
;;   '("tamura70" "http://d.hatena.ne.jp/tamura70/rdf"
;;     "~/org/rdf.org" "屯遁"
;;     :parse-feed org-feed-parse-rdf-feed))

;; (add-to-list 'org-feed-alist
;;   '("game" "http://rdfblog.ameba.jp/get6-2/rdf20.xml"
;;     "~/org/rdf.org" "game"
;;     :parse-feed org-feed-parse-rdf-feed))

(provide 'init-org)
