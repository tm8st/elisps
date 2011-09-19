;;; init-org.el --- org mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>g
;; Keywords: init, org
;; creation time: Wed Apr 28 00:53:29 2010
;;; Commentary:

;;; Code:

(my-require 'org)
(my-require 'org-install)
(my-require 'org-agenda)
(my-require 'org-mobile)
(my-require 'org-habit)
(my-require 'org-capture)
(my-require 'org-clock)
(my-require 'init-keybindings)

(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(setq org-log-done 'time) ;; DONEの時刻を記録
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

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

(my-require 'auto-complete)
(add-to-list 'ac-modes 'org-mode)

(customize-set-variable 'org-agenda-include-diary t)

(my-require 'simple)
(define-key org-mode-map (kbd "C-j") 'newline)
(define-key org-mode-map (kbd "C-c C-j") 'org-return-indent)

(setq org-directory (concat my-dropbox-directory "Org/"))
(setq org-private-directory "~/Org/")

(setq org-default-notes-file (concat org-directory "life.org"))
(setq org-index-notes-file (concat org-private-directory "main.org"))
(setq org-life-notes-file (concat org-directory "life.org"))
(setq org-inbox-notes-file (concat org-directory "inbox.org"))
(setq org-code-notes-file (concat org-directory "code.org"))

(setq org-agenda-files
      (list org-index-notes-file
            org-life-notes-file
            org-inbox-notes-file
            org-code-notes-file))

;; TODOの状態遷移
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w!)" "|" "DONE(d)" "SOMEDAY(s) " "CANCELD(c)")))

(customize-set-variable
 'org-tag-alist
 '(("WORK" . ?w)
   ("EMACS" . ?e)
   ("LIFE" . ?l)
   ("GAME" . ?g)
   ("UE3" . ?u)
   ("ANIME" . ?a)
   ("MEMO" . ?m)
   ))

;; DONEの時刻を記録
(setq org-log-done 'time)

(setq org-capture-templates
      '(
        ("t" "TASK" entry (file+headline org-index-notes-file "TASKS")
         "** TASK \n  %i\n  %a\n  %U\n")
        ("i" "IDEA" entry (file+headline org-index-notes-file "IDEAS")
         "** SOMEDAY \n  %i\n  %a\n  %U\n")
        ("m" "MEMO" entry (file+headline org-index-notes-file "NOTES")
         "** \n  %i\n  %a\n  %U\n")

        ("l" "LIFE-TASK" entry (file+headline org-life-notes-file "LIFE")
         "** TASK \n  %i\n  %a\n  %U\n")
        (";" "LIFE-IDEA" entry (file+headline org-life-notes-file "IDEAS")
         "** SOMEDAY \n  %i\n  %a\n  %U\n")
        (":" "LIFE-MEMO" entry (file+headline org-life-notes-file "NOTES")
         "** \n  %i\n  %a\n  %U\n")
        ("h" "LIFE-HABIT" entry (file+headline org-life-notes-file "HABIT")
         "** \n  :PROPERTIES:\n  :LOGGING: DONE(!) logrepeat\n  :END:\n	%i\n  %a\n  %U\n")

        ("d" "INBOX-IDEA" entry (file+headline org-inbox-notes-file "IDEAS")
         "** SOMEDAY \n  %i\n  %a\n  %U\n")

        ("f" "INBOX-MEMO" entry (file+headline org-inbox-notes-file "NOTES")
         "** \n  %i\n  %a\n  %U\n")

        ("r" "REVIEW" entry (file+headline org-inbox-notes-file "REVIEW")
         "** \n  %i\n  %a\n  %U\n")

        ("c" "CODE" entry (file+headline org-code-notes-file "CODE")
         "** \n  %i\n  %a\n  %U\n")
        ))

;; MobileOrg
(setq org-mobile-inbox-for-pull (concat org-directory "pulled.org"))
;; Dropbox直下のMobileOrgフォルダへのパスを設定
(setq org-mobile-directory (concat my-dropbox-directory "MobileOrg"))
(setq org-mobile-force-id-on-agenda-items t)

;; 標準の祝日を利用する
(customize-set-variable 'org-calendar-holidays t)

;; キーバインドの設定
(define-key global-map (kbd "C-l C-o C-r") 'org-capture)

;; index fileを開く
(defun my-org-open-index ()
  (interactive)
  (find-file org-index-notes-file))
(defun my-org-open-life ()
  (interactive)
  (find-file org-life-notes-file))
(defun my-org-open-inbox ()
  (interactive)
  (find-file org-inbox-notes-file))

(define-key global-map (kbd "C-l C-o C-o") 'my-org-open-index)
(define-key global-map (kbd "C-l C-o C-l") 'my-org-open-life)
(define-key global-map (kbd "C-l C-o C-i") 'my-org-open-inbox)
(define-key global-map (kbd "C-l C-o C-s") 'org-store-link)

;; (defun my-org-mobile-sync ()
;; 	(org-mobile-pull)
;; 	(org-mobile-push))

(define-key global-map (kbd "C-l C-o C-p") 'org-mobile-push)
(define-key global-map (kbd "C-l C-o C-@") 'org-mobile-pull)

(define-key org-mode-map (kbd "C-k") 'my-delete-line-forward)
(define-key org-mode-map (kbd "C-m") my-backward-word-command)
(define-key org-mode-map (kbd "C-TAB") 'org-force-cycle-archived)
(define-key org-mode-map [(meta left)]  'org-metaleft)
(define-key org-mode-map [(meta right)] 'org-metaright)
(define-key org-mode-map [(meta up)]    'org-metaup)
(define-key org-mode-map [(meta down)]  'org-metadown)

(define-key org-mode-map (kbd "M-N") 'org-metadown)
(define-key org-mode-map (kbd "M-P") 'org-metaup)
(define-key org-mode-map (kbd "M-n") 'org-forward-same-level)
(define-key org-mode-map (kbd "M-p") 'org-backward-same-level)
(define-key org-mode-map (kbd "C-m") my-backward-word-command)
(define-key org-mode-map (kbd "C-M-m") 'bm-toggle)
(define-key org-mode-map (kbd "C-M-j") 'org-insert-heading)

;;;-------------------------------
;;; org-agenda setting.
;;;-------------------------------
(my-require 'org-agenda)
(add-hook 'org-agenda-mode-hook
          '(lambda ()
             (hl-line-mode 1)
             (setq hl-line-face 'underline)))

(setq org-stuck-projects
      '("+LEVEL=2/-DONE"
        ("TASK" "CANCELD")
        ("MEMO") ""))

(setq org-agenda-prefix-format
  '((agenda  . " %i %-8:c%?-8t% s")
    (timeline  . "  % s")
    (todo  . " %i %-8:c")
    (tags  . " %i %-8:c")
    (search . " %i %-8:c")))

(define-key global-map (kbd "C-l C-o C-a") 'org-agenda)

;; (define-key global-map (kbd "C-l C-o C-l") 'org-agenda-list)

(my-require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*Org Agenda*" :height 0.5))

(my-require 'tm8st-growl)
(defun my-org-agenda-open-buffer ()
  (interactive)
  (display-buffer "*Org Agenda*"))

(define-key global-map (kbd "C-l C-o C-z") 'my-org-agenda-open-buffer)

(defun my-org-clock-in ()
  (interactive)
  (tm8st-growl-timer "Org" "Org" nil (* 60 1000))
  (bm-toggle)
  (org-clock-in))

(defun my-org-clock-out ()
  (interactive)
  (bm-toggle)
  (se-play (concat my-dropbox-directory "Emacs/SE/amivoice/otsukaresamaN.mp3"))
  (tm8st-growl-notify-delete-sticky "DONE!" "Org")
  (org-clock-out))

(defun my-org-clock-cancel ()
  (interactive)
  (tm8st-growl-timer-cancel)
  (org-clock-out))

;; timer start, stop.
(define-key org-mode-map (kbd "C-c C-i") 'my-org-clock-in)
(define-key org-mode-map (kbd "C-c C-o") 'my-org-clock-out)
(define-key org-mode-map (kbd "C-c C-q") 'org-clock-display)
(define-key org-mode-map (kbd "C-c C-r") 'org-clock-report)

(global-set-key (kbd "C-l C-o C-c") 'my-org-clock-cancel)

(define-key org-agenda-mode-map (kbd "C-i") 'org-agenda-clock-in)
(define-key org-agenda-mode-map (kbd "C-o") 'org-agenda-clock-out)

(defun my-add-separate-char-on-newline ()
  "ざっくりとレイアウト調整。"
  (interactive)
  (let ((s (point))
        (e (mark)))
    (replace-string "," ",\n" nil s e)
    (replace-string "\." ".\n" nil s e)
    (replace-string "!" "!\n" nil s e)
    (replace-string "、" "、\n" nil s e)
    (replace-string "。" "。\n" nil s e)
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
(my-require 'org-checklist)
(setq org-reverse-note-order nil)
(my-require 'org-crypt)
(org-crypt-use-before-save-magic); Encrypt all entries before saving
(setq org-tags-exclude-from-inheritance (quote ("CRYPT"))); Which tag is used to mark headings to be encrypted
(customize-set-value 'org-crypt-tag-matcher "CRYPT")
(setq org-crypt-key "F0B66B40"); GPG key to use for encryption

;; おちる。。。
;; (defun my-org-hide-other-subtree ()
;;   (interactive)
;;   (org-narrow-to-subtree)
;;   ;; (org-show-todo-tree nil)
;;   )
;; (global-set-key (kbd "C-l C-o C-v") 'my-org-hide-other-subtree)

(when (my-is-windows)
  (setq tm8st-growl-type 'windows-growlnotify))

(when (my-is-mac)
  (setq tm8st-growl-type 'mac-growlnotify))

;; (my-require 'org-export-hatena)
;; (my-require 'simple-hatena-mode)

(defun my-org-mode-hook ()
  (yalinum-mode -1)
  )

(add-hook 'org-mode-hook 'my-org-mode-hook)

(provide 'init-org)
