;;; init-haskell.el --- haskell mode setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, haskell
;; creation time: [Sun Aug  1 18:12:46 2010]
;;; Commentary:

(require 'haskell-mode)
(require 'haskell-indentation)

(setq auto-mode-alist
      (append
       '(("\\.hs$" . haskell-mode)
	 )
       auto-mode-alist))

(setq haskell-literate "haskell-mode")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; (add-hook 'haskell-mode-hook '(lambda () ()))

(define-key haskell-mode-map (kbd "C-m") `backward-word)
(define-key haskell-indentation-mode-map (kbd "C-j") `haskell-newline-and-indent)
(define-key haskell-indentation-mode-map (kbd "C-m") `backward-word)
;; (define-key haskell-mode-map (kbd "C-m") `my-backward-word)
;; (define-key haskell-indentation-mode-map (kbd "C-") `haskell-newline-and-indent)

(provide 'init-haskell)
