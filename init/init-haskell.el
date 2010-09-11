;;; init-haskell.el --- haskell mode setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, haskell
;; creation time: [Sun Aug  1 18:12:46 2010]
;;; Commentary:

(require 'haskell-mode)
(require 'haskell-indentation)
(require 'inf-haskell)

(setq auto-mode-alist
      (append
       '(("\\.hs$" . haskell-mode)
	 )
       auto-mode-alist))

(setq haskell-literate "haskell-mode")

;; haskellでauto-compoleを使う
(add-to-list 'ac-modes 'haskell-mode)

(defun my-haskell-mode-hook ()
  (interactive)
  (setq indent-tabs-mode nil)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (turn-on-haskell-indent)
  (turn-on-haskell-simple-indent)
  (global-auto-complete-mode t)
  (auto-complete-mode t)
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-imenu))
  )

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

(define-key haskell-mode-map (kbd "C-m") 'backward-word)
(define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)
(define-key haskell-indentation-mode-map (kbd "C-j") 'haskell-newline-and-indent)
(define-key haskell-indentation-mode-map (kbd "C-m") 'backward-word)

;; (define-key haskell-mode-map (kbd "C-m") `my-backward-word)
;; (define-key haskell-indentation-mode-map (kbd "C-") `haskell-newline-and-indent)

(provide 'init-haskell)
