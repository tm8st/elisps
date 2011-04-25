;;; init-yasnippet.el --- yasnippet setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, yasnippet
;; creation time: Mon May  3 00:41:37 2010
;;; Commentary:

;;; Code:

(require 'yasnippet)

(defun my-update-yasnippet-files ()
  "ファイル更新時用に読み込み直し"
  (interactive)
  (yas/load-directory my-yas/load-directory)
  )

;; メニューは使わない
(setq yas/use-menu nil)
;; トリガは使わない
(setq yas/trigger-key "")

(global-set-key (kbd "C-q TAB") 'yas/expand)
(global-set-key (kbd "C-l C-y C-l") 'my-update-yasnippet-files)
(global-set-key (kbd "C-l C-y C-n") 'yas/new-snippet)
(global-set-key (kbd "C-l C-y C-v") 'yas/visit-snippet-file)

(yas/initialize)
(my-update-yasnippet-files)

(setq yas/buffer-local-condition
      '(or (not (or (string= "font-lock-comment-face"
                             (get-char-property (point) 'face))
                    (string= "font-lock-string-face"
                             (get-char-property (point) 'face))))
           '(require-snippet-condition . force-in-comment)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda () (yas/minor-mode-on)))
(add-hook 'c++-mode-hook
          '(lambda () (yas/minor-mode-on)))
(add-hook 'objc-mode-hook
          '(lambda () (yas/minor-mode-on)))
(add-hook 'ruby-mode-hook
          '(lambda () (yas/minor-mode-on)))
(add-hook 'haskell-mode-hook
          '(lambda () (yas/minor-mode-on)))

(provide 'init-yasnippet)
