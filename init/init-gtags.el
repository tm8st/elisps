;;; init-gtags.el --- gtags setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, gtags
;; creation time: Wed Apr 28 00:14:13 2010
;;; Commentary:

;;; Code:

;;GNU GLOBAL(gtags)
(require 'gtags)
(autoload 'gtags-mode "gtags" "" t)
(gtags-mode t)
(customize-set-value 'gtags-path-style 'relative)

(gtags-make-complete-list)

(defun my-gtags-update-tags ()
  ""
  (interactive)
  (message (concat "Update " (gtags-get-rootpath) " gtags files."))
  (start-process-shell-command "gtags-update" "*my-gtags-update*" (concat "cd " (gtags-get-rootpath) " && gtags -v"))
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
(global-set-key (kbd "C-q C-h") 'gtags-find-tag)

(provide 'init-gtags)
