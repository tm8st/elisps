;;; init-haskell.el --- haskell mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, haskell
;; creation time: [Sun Aug  1 18:12:46 2010]
;;; Commentary:

;; (require 'hs)
;; (add-to-list 'auto-mode-alist '("\\.hs$" . hs-mode))
;; (add-to-list 'auto-mode-alist '("\\.hsc$" . hs-mode))

(require 'yalinum)
(require 'haskell-mode)
(require 'haskell-indentation)
(require 'inf-haskell)
(require 'prefix-arg-commands)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hsc$" . haskell-mode))

(require 'auto-complete)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'inferior-haskell-mode)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*GHC Errors*" :height 0.3))
;; (add-to-list 'popwin:special-display-config '("*haskell*" :height 0.2 :noselect))  ;; 勝手に閉じてしまう. popwin.el 0.2
;; (add-to-list 'popwin:special-display-config '("*hs-lint*")) ;; 勝手に閉じてしまう. popwin.el 0.2
;; (setq popwin:special-display-config '(("*haskell*" :height 0.4 :noselect)))

(require 'ghc)

(define-key haskell-mode-map (kbd "C-c C-o") 'ghc-complete)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'ghc-browse-document)
(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
(define-key haskell-mode-map (kbd "C-c C-s") 'ghc-save-buffer)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(define-key haskell-mode-map (kbd "C-m") 'backward-word)
(define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)

(prefix-arg-commands-defun prefix-arg-commands-insert-haskell-right-arrow
                           (list
                            '(lambda () (interactive) (insert-string " -> "))
                            '(lambda () (interactive) (insert-string " => "))))

(define-key haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert-string " <- ")))
(define-key haskell-mode-map (kbd "C-c C-r") `prefix-arg-commands-insert-haskell-right-arrow)
(define-key haskell-mode-map (kbd "C-c C-@") '(lambda () (interactive) (insert-string "``") (backward-char)))
(define-key haskell-mode-map (kbd "C-c C-1") '(lambda () (interactive) (insert-string "!!") (backward-char)))

(define-key inferior-haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert-string " <- ")))
(define-key inferior-haskell-mode-map (kbd "C-c C-r") `prefix-arg-commands-insert-haskell-right-arrow)
(define-key inferior-haskell-mode-map (kbd "C-c C-@") '(lambda () (interactive) (insert-string "``") (backward-char)))
(define-key inferior-haskell-mode-map (kbd "C-c C-1") '(lambda () (interactive) (insert-string "!!") (backward-char)))

(define-key haskell-mode-map (kbd "C-c C-c") 'inferior-haskell-load-file)
(define-key haskell-mode-map (kbd "C-c C-e") 'inferior-haskell-load-and-run)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)
(define-key haskell-mode-map (kbd "C-c s") 'hs-scan)
(define-key haskell-mode-map (kbd "C-o") 'auto-complete)

(define-key haskell-indentation-mode-map (kbd "C-j") 'haskell-newline-and-indent)
(define-key haskell-indentation-mode-map (kbd "C-m") 'backward-word)

(define-key inferior-haskell-mode-map (kbd "C-m") 'backward-word)
(define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)
(define-key inferior-haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)

(require 'hs-lint) ; Hackage hlint

;; (require 'scion)   ; Emacs wiki "scion"
;; (local-set-key "\C-c\C-x." 'scion-goto-definition)
;; (require 'hs-scan) ; google it 'Haskell style scanner'

;;
(defun my-haskell-mode-hook ()
  (progn
    (auto-complete-mode t)
    ;; (scion-mode t)
    ;; (scion-flycheck-on-save nil) ; conflict with auto-buffer-save
    ;; (setq scion-completing-read-function 'ido-completing-read)
    (ghc-init)
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (yalinum-mode t)
    ))

(defun my-haskell-cabal-mode ()
  (progn
    (setq tab-width 2 indent-tabs-mode nil)
    ))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode)

(require 'haskell-move-nested)
(define-key haskell-mode-map (kbd "C-S-B")
  (lambda ()
    (interactive)
    (haskell-move-nested -1)))

(define-key haskell-mode-map (kbd "C-S-F")
  (lambda ()
    (interactive)
    (haskell-move-nested 1)))

(provide 'init-haskell)
