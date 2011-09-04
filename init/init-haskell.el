;;; init-haskell.el --- haskell mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, haskell
;; creation time: [Sun Aug  1 18:12:46 2010]
;;; Commentary:

;; (my-require 'hs)
;; (add-to-list 'auto-mode-alist '("\\.hs$" . hs-mode))
;; (add-to-list 'auto-mode-alist '("\\.hsc$" . hs-mode))

(my-require 'yalinum)
(my-require 'haskell-mode)
(my-require 'haskell-indentation)
(my-require 'inf-haskell)
(my-require 'prefix-arg-commands)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hsc$" . haskell-mode))

(my-require 'auto-complete)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'inferior-haskell-mode)

(my-require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*GHC Errors*" :height 0.3))
;; (add-to-list 'popwin:special-display-config '("*haskell*" :height 0.2 :noselect))  ;; 勝手に閉じてしまう. popwin.el 0.2
;; (add-to-list 'popwin:special-display-config '("*hs-lint*")) ;; 勝手に閉じてしまう. popwin.el 0.2
;; (setq popwin:special-display-config '(("*haskell*" :height 0.4 :noselect)))

(my-require 'ghc)
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
                            '(lambda () (interactive) (insert " -> "))
                            '(lambda () (interactive) (insert " => "))))

(defun my-hs-if-previous-line-function-define-put-func-name ()
  (interactive)
  (let ((limitPos (point))
        (funcName nil))
    (save-excursion
      (line-move -1)
      (beginning-of-line)
      (unless (eq (search-forward "::" limitPos 1) nil)
        (progn
          (back-to-indentation)
          (setq funcName (thing-at-point 'word)))))
    (unless (eq funcName nil)
      (insert funcName))))

(define-key haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert " <- ")))
(define-key haskell-mode-map (kbd "C-c C-r") `prefix-arg-commands-insert-haskell-right-arrow)
(define-key haskell-mode-map (kbd "C-c C-@") '(lambda () (interactive) (insert " `` ") (backward-char 2)))
(define-key haskell-mode-map (kbd "C-c C-1") '(lambda () (interactive) (insert " !! ") (backward-char 2)))
(define-key haskell-mode-map (kbd "C-c C--") '(lambda () (interactive) (insert " = ")))
(define-key haskell-mode-map (kbd "C-c C-j") 'my-hs-if-previous-line-function-define-put-func-name)

(define-key inferior-haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert " <- ")))
(define-key inferior-haskell-mode-map (kbd "C-c C-r") `prefix-arg-commands-insert-haskell-right-arrow)
(define-key inferior-haskell-mode-map (kbd "C-c C-@") '(lambda () (interactive) (insert " `` ") (backward-char 2)))
(define-key inferior-haskell-mode-map (kbd "C-c C-1") '(lambda () (interactive) (insert " !! ") (backward-char 2)))
(define-key inferior-haskell-mode-map (kbd "C-c C--") '(lambda () (interactive) (insert " = ")))

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

(my-require 'hs-lint) ; Hackage hlint

;; (my-require 'scion)   ; Emacs wiki "scion"
;; (local-set-key "\C-c\C-x." 'scion-goto-definition)
;; (my-require 'hs-scan) ; google it 'Haskell style scanner'

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

(my-require 'haskell-move-nested)
(define-key haskell-mode-map (kbd "C-S-B")
  (lambda ()
    (interactive)
    (haskell-move-nested -1)))

(define-key haskell-mode-map (kbd "C-S-F")
  (lambda ()
    (interactive)
    (haskell-move-nested 1)))

(provide 'init-haskell)
