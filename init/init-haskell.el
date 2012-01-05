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
(my-require 'init-keybindings)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.hsc$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))
(add-to-list 'interpreter-mode-alist '("runghc" . haskell-mode))     ;#!/usr/bin/env runghc 用
(add-to-list 'interpreter-mode-alist '("runhaskell" . haskell-mode)) ;#!/usr/bin/env runhaskell 用

(my-require 'auto-complete)
(add-to-list 'ac-modes 'haskell-mode)
(add-to-list 'ac-modes 'inferior-haskell-mode)

(my-require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*GHC Errors*" :height 0.3))
;; (add-to-list 'popwin:special-display-config '("*haskell*" :height 0.2 :noselect))  ;; 勝手に閉じてしまう. popwin.el 0.2
;; (add-to-list 'popwin:special-display-config '("*hs-lint*")) ;; 勝手に閉じてしまう. popwin.el 0.2
;; (setq popwin:special-display-config '(("*haskell*" :height 0.4 :noselect)))

(my-require 'ghc)

(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

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

(defadvice haskell-indent-indentation-info (after haskell-indent-reverse-indentation-info)
  (when (>= (length ad-return-value) 2)
    (let ((second (nth 1 ad-return-value)))
      (setq ad-return-value (cons second (delete second ad-return-value))))))

(ad-activate 'haskell-indent-indentation-info)

;;;-------------------------------
;;; wakaran-san
;;;-------------------------------
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(defvar anything-c-source-ghc-mod
  '((name . "ghc-browse-document")
    (init . anything-c-source-ghc-mod)
    (candidates-in-buffer)
    (candidate-number-limit . 9999999)
    (action ("Open" . anything-c-source-ghc-mod-action))))

(defun anything-c-source-ghc-mod ()
  (unless (executable-find "ghc-mod")
    (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
  (let ((buffer (anything-candidate-buffer 'global)))
    (with-current-buffer buffer
      (call-process "ghc-mod" nil t t "list"))))

(defun anything-c-source-ghc-mod-action (candidate)
  (interactive "P")
  (let* ((pkg (ghc-resolve-package-name candidate)))
    (if (and pkg candidate)
        (ghc-display-document pkg "ghc-mod" nil)
      (message "No document found"))))

(defun anything-ghc-browse-document ()
  (interactive)
  (anything anything-c-source-ghc-mod))

(prefix-arg-commands-defun prefix-arg-commands-insert-haskell-right-arrow
                           (list
                            '(lambda () (interactive) (insert " -> "))
                            '(lambda () (interactive) (insert " => "))))

(prefix-arg-commands-defun prefix-arg-commands-insert-haskell-minus
                           (list
                            '(lambda () (interactive) (insert " = "))
                            '(lambda () (interactive) (insert " >>= "))
                            '(lambda () (interactive) (insert " >> "))))

(defun my-hs-if-previous-line-function-define-put-func-name ()
  (interactive)
  (let ((limitPos (point))
        (funcName nil))
    (save-excursion
      (line-move -1)
      (beginning-of-line)
      (unless (eq (search-forward "::" limitPos 1) nil)
          (back-to-indentation)
          (setq funcName (thing-at-point 'word))))
    (unless (eq funcName nil)
      (insert funcName))))

(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod)))
(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(defun my-haskell-ac-init ()
  (when (member (file-name-extension buffer-file-name) '("hs" "lhs"))
    (auto-complete-mode t)
    (setq ac-sources '(ac-source-words-in-same-mode-buffers ac-source-dictionary ac-source-ghc-mod))))

(add-hook 'haskell-mode-hook 'my-haskell-ac-init)

(define-key haskell-mode-map (kbd "C-S-F")
  (lambda ()
    (interactive)
    (haskell-move-nested 1)))

(defadvice haskell-indent-indentation-info (after haskell-indent-reverse-indentation-info)
  (when (>= (length ad-return-value) 2)
    (let ((second (nth 1 ad-return-value)))
      (setq ad-return-value (cons second (delete second ad-return-value))))))

(ad-activate 'haskell-indent-indentation-info)

(require 'anything)
(require 'anything-hasktags)
;; make tags command.
;; alias hasktags-r="find . -type f -name \*.\*hs -print0 | xargs -0 hasktags -c"

(define-key haskell-mode-map (kbd "C-q C-j") 'anything-hasktags-select)

(defun my-update-haskell-tags ()
  (interactive)
  (async-shell-command
   (concat "cd \"" (file-name-directory (buffer-file-name))
           "\" && find . -type f -name \*.\*hs -print0 | xargs -0 hasktags -c") "*Update Hasktags*" nil))

(defun my-launch-client ()
  (interactive)
  (async-shell-command
   (concat "cd \"" (file-name-directory (buffer-file-name))
           "\" && runhaskell aclient") "*AClient*" nil))

(defun my-launch-server ()
  (interactive)
  (async-shell-command
   (concat "cd " (file-name-directory (buffer-file-name))
           " && runhaskell server") "*Server*" nil))

(define-key haskell-mode-map (kbd "C-c C-o") 'ghc-complete)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'ghc-browse-document)
(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
(define-key haskell-mode-map (kbd "C-c C-s") 'ghc-save-buffer)

(define-key haskell-mode-map (kbd "C-c C-o") 'ghc-complete)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'anything-ghc-browse-document)
(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
(define-key haskell-mode-map (kbd "C-c C-s") 'ghc-save-buffer)

(define-key haskell-mode-map (kbd "C-c C-a") '(lambda () (interactive) (insert " <*> ")))
(define-key haskell-mode-map (kbd "C-c C-f") '(lambda () (interactive) (insert " <$> ")))
(define-key haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert " <- ")))
(define-key haskell-mode-map (kbd "C-c C-r") `prefix-arg-commands-insert-haskell-right-arrow)
(define-key haskell-mode-map (kbd "C-c C-@") '(lambda () (interactive) (insert " `` ") (backward-char 2)))
(define-key haskell-mode-map (kbd "C-c C-1") '(lambda () (interactive) (insert " !! ") (backward-char 2)))
(define-key haskell-mode-map (kbd "C-c C-j") 'my-hs-if-previous-line-function-define-put-func-name)
(define-key haskell-mode-map (kbd "C-c C--") 'prefix-arg-commands-insert-haskell-minus)

(define-key inferior-haskell-mode-map (kbd "C-c C-a") '(lambda () (interactive) (insert " <*> ")))
(define-key inferior-haskell-mode-map (kbd "C-c C-f") '(lambda () (interactive) (insert " <$> ")))
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
(define-key haskell-indentation-mode-map (kbd "C-m") my-backward-word-command)

(define-key inferior-haskell-mode-map (kbd "C-m") my-backward-word-command)
(define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)
(define-key inferior-haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)

(define-key haskell-mode-map (kbd "C-c C-e") 'my-launch-client)
(define-key haskell-mode-map (kbd "C-c C-s") 'my-launch-server)

(define-key haskell-mode-map (kbd "C-m") my-backward-word-command)
(define-key haskell-mode-map (kbd "C-c C-h") 'haskell-hoogle)

(provide 'init-haskell)
