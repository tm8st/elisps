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
       '(("\\.hs$" . haskell-mode))
       auto-mode-alist))

(setq haskell-literate "haskell-mode")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

;; haskellでauto-compoleを使う
(require 'auto-complete)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'inferior-haskell-mode)

;;for haskell-mode                                                                                  
(autoload 'ghc-init "ghc" nil t)

(defun haskell-individual-setup ()
  (let ((mapping '(([f5] . "\C-c\C-l\C-x\omain\C-m\C-xo")
                   ("\C-c\C-i" . ghc-complete)
                   ([backtab] . haskell-indent-cycle))))    
    (loop for (key . f) in mapping
          do (define-key haskell-mode-map key f))
    
    (turn-on-haskell-doc-mode)
    (turn-on-haskell-indent)
    (imenu-add-menubar-index)
    (ghc-init)
    (flymake-mode)))

(add-hook 'haskell-mode-hook 'haskell-individual-setup)

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
(define-key inferior-haskell-mode-map (kbd "C-m") 'backward-word)
(define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)

;; (define-key haskell-mode-map (kbd "C-m") `my-backward-word)
;; (define-key haskell-indentation-mode-map (kbd "C-") `haskell-newline-and-indent)

(provide 'init-haskell)
