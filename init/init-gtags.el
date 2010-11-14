;;; init-gtags.el --- gtags setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, gtags
;; creation time: Wed Apr 28 00:14:13 2010
;;; Commentary:

;;; Code:

;;GNU GLOBAL(gtags)
(require 'gtags)
(gtags-mode t)
(customize-set-value 'gtags-path-style 'relative)

(gtags-make-complete-list)

(defun my-gtags-update-tags ()
 ""
  (interactive)
  (message (concat "Update " (gtags-get-rootpath) " gtags files."))
  (start-process-shell-command
   "gtags-update"
   "*my-gtags-update*"
   (concat "cd " (gtags-get-rootpath) " && gtags -v"))
  )

(global-set-key (kbd "C-q C-e")  'my-gtags-update-tags)

;; 手軽に使えるようなキーバインド 
(global-set-key (kbd "C-q C-n")  'gtags-find-tag)
(global-set-key (kbd "C-q C-m")  'gtags-find-rtag)
(global-set-key (kbd "C-q C-j") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-q C-f C-g") 'gtags-find-with-grep)
(global-set-key (kbd "C-q C-f C-f") 'gtags-find-file)
(global-set-key (kbd "C-q C-f C-s")'gtags-find-symbol)
(global-set-key (kbd "C-q C-p") 'gtags-pop-stack)

;;;-------------------------------
;;; etags 
;;;-------------------------------
(require 'etags)
(require 'anything-etags)
(setq my-etags-command "ctags -R")
;; (setq my-etags-command "find . -name \"*.*\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -a -not -name \"*.exe\" -exec etags -a {} +")
(defun my-etags-update ()
  (interactive)
  (async-shell-command my-etags-command "*etags update*" nil)
  )
(global-set-key (kbd "C-q C-a C-e") 'anything-etags-select-from-here)
(global-set-key (kbd "C-q C-a C-w") 'anything-etags-select)
(global-set-key (kbd "C-q C-a C-n") 'anything-etags-select-from-here)
(global-set-key (kbd "C-q C-a C-m") 'anything-etags-select)
;; (global-set-key (kbd "C-l C-j C-u") 'my-etags-update)

(provide 'init-gtags)
