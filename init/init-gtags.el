;;; init-gtags.el --- gtags setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, gtags
;; creation time: Wed Apr 28 00:14:13 2010
;;; Commentary:

;;; Code:

;;GNU GLOBAL(gtags)
(require 'gtags)
(gtags-mode t)
(gtags-make-complete-list)
(customize-set-value 'gtags-path-style 'relative)

(defun my-gtags-update-tags ()
 ""
  (interactive)
  (message (concat "Update " (gtags-get-rootpath) " gtags files."))
  (start-process-shell-command
   "gtags-update"
   "*my-gtags-update*"
   (concat "cd " (gtags-get-rootpath) " && gtags -v"))
  )

(global-set-key (kbd "C-q C-e") 'my-gtags-update-tags)

;; 手軽に使えるようなキーバインド
(global-set-key (kbd "C-q C-n") 'gtags-find-tag)
(global-set-key (kbd "C-q C-m") 'gtags-find-rtag)
(global-set-key (kbd "C-q C-j") 'gtags-find-tag-from-here)
(global-set-key (kbd "C-q C-f C-g") 'gtags-find-with-grep)
(global-set-key (kbd "C-q C-f C-f") 'gtags-find-file)
(global-set-key (kbd "C-q C-f C-s")'gtags-find-symbol)
(global-set-key (kbd "C-q C-p") 'gtags-pop-stack)

;;;-------------------------------
;;; etags 
;;;-------------------------------
(require 'etags)
;; (require 'anything-etags) obsolete
(require 'anything-config)
(defvar my-etags-command "ctags -e --recurse")
;; (setq my-etags-command "find . -name \"*.*\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -a -not -name \"*.exe\" -exec etags -a {} +")

;; get the path of gtags root directory.
(defun my-etags-get-rootpath ()
  (anything-etags-find-tag-file (file-name-directory (buffer-file-name))))

(defun my-etags-update ()
  (interactive)
  (async-shell-command (concat "cd \"" (my-etags-get-rootpath) "\" " my-etags-command "*etags update*" nil)))

(global-set-key (kbd "C-q C-a C-e") 'anything-etags-select-from-here)
(global-set-key (kbd "C-q C-a C-w") 'anything-etags-select)
;; (global-set-key (kbd "C-q C-a C-n") 'anything-etags-select-from-here)
;; (global-set-key (kbd "C-q C-a C-m") 'anything-etags-select)
;; (global-set-key (kbd "C-l C-j C-u") 'my-etags-update)

;; (require 'virtual-tags)
;; (global-set-key (kbd "C-q C-e") 'virtual-tags-update-tags)
;; (global-set-key (kbd "C-q C-@") 'virtual-tags-init-tags)
;; (global-set-key (kbd "C-q C-j") 'virtual-tags-find-tags-from-here)
;; (global-set-key (kbd "C-q C-m") 'virtual-tags-find-tags)

(provide 'init-gtags)
