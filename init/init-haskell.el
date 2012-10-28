;;; init-haskell.el --- haskell mode setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, haskell
;; creation time: [Sun Aug  1 18:12:46 2010]
;;; Commentary:

;; (my-require 'hs)

(my-require 'init-compile-env)
(my-require 'yalinum)
(my-require 'haskell-mode)
(my-require 'haskell-indentation)
(my-require 'inf-haskell)
(my-require 'prefix-arg-commands)
(my-require 'init-keybindings)
(my-require 'auto-complete)
(my-require 'sub-frame)
(my-require 'sub-frame-config)
(my-require 'hamlet-mode)
(my-require 'ghc)
(my-require 'hs-lint) ; Hackage hlint

;;;-------------------------------
;;; auto mode alist settings.
;;;-------------------------------
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
;; 勝手に閉じてしまうので、設定しない
;; (add-to-list 'popwin:special-display-config '("*GHC Errors*" :height 0.3))
;; (add-to-list 'popwin:special-display-config '("*haskell*" :height 0.2 :noselect))
;; (add-to-list 'popwin:special-display-config '("*hs-lint*"))
;; (setq popwin:special-display-config '(("*haskell*" :height 0.4 :noselect)))

;;
(defun my-haskell-mode-hook ()
  (progn
    (auto-complete-mode t)
    (when (my-is-windows)
      (setq ghc-module-command "ghc-mod"))
    (setq ghc-flymake-command nil)

    (ghc-init)
    (flymake-mode)
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (turn-on-haskell-indent)
    (turn-on-haskell-doc-mode)
    (font-lock-mode)
    (imenu-add-menubar-index)

    (highlight-indentation-mode t)
    (highlight-indentation-current-column-mode t)
    (yalinum-mode t)

    ;; override ghc-mod key binding.
    (define-key haskell-mode-map (kbd "C-x C-s") 'my-hs-save-buffer)

    (define-key haskell-mode-map (kbd "C-c C-c") 'my-inferior-haskell-load-file)
    ))

(defun my-haskell-cabal-mode-hook ()
  (progn
    (setq tab-width 2 indent-tabs-mode nil)
    ))

(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'haskell-cabal-mode-hook 'my-haskell-cabal-mode-hook)

(my-require 'haskell-move-nested)
(define-key haskell-mode-map (kbd "C-S-B") (lambda () (interactive) (haskell-move-nested -1)))
(define-key haskell-mode-map (kbd "C-S-F") (lambda () (interactive) (haskell-move-nested 1)))

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

(ac-define-source ghc-mod
 '((depends ghc)
   (candidates . (ghc-select-completion-symbol))
   (symbol . "g")
   (cache)))

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

(defun my-ac-haskell-mode ()
  (setq ac-sources '(ac-source-ghc-mod
                     ac-source-words-in-same-mode-buffers
                     )))

(add-hook 'haskell-mode-hook 'my-ac-haskell-mode)

(my-require 'anything)
(my-require 'anything-hasktags)

(define-key haskell-mode-map (kbd "C-q C-j") 'anything-hasktags-select-from-here)
(define-key haskell-mode-map (kbd "C-q C-e") 'my-update-haskell-tags)

(defun my-update-haskell-tags ()
  (interactive)
  (sf:async-shell-command
   (concat "cd \""
           (file-name-directory (buffer-file-name))
           "\" && find . -type f -name \*.\*hs -print0 | xargs -0 hasktags -c") "*Update Hasktags*" nil))

(defun my-run-haskell-buffer-file ()
  (interactive)
  (save-buffer)
  (sf:async-shell-command
   (concat "cd " (file-name-directory (buffer-file-name))
           " && runhaskell " (file-name-nondirectory (buffer-file-name)))
   (concat "*RunHaskell "(file-name-nondirectory (buffer-file-name)) "*") nil))

(defun my-run-haskell-buffer-file-with-args ()
  (interactive)
  (save-buffer)
  (sf:async-shell-command
   (concat "cd " (file-name-directory (buffer-file-name))
           " && runhaskell " (file-name-nondirectory (buffer-file-name)) " " (read-string "args: "))
   (concat "*RunHaskell "(file-name-nondirectory (buffer-file-name)) "*") nil))

(defun my-doctest-buffer-file ()
  (interactive)
  (sf:async-shell-command
   (concat "cd " (file-name-directory (buffer-file-name))
           " && doctest " (file-name-nondirectory (buffer-file-name)))
   (concat "*doctest "(file-name-nondirectory (buffer-file-name)) "*") nil))

;;;-------------------------------
;;; haskell online game debug launcher
;;;-------------------------------
(defun my-runhaskell-same-buffer-file (name)
  (interactive)
  (sf:async-shell-command
   (concat "cd \"" (file-name-directory (buffer-file-name))
           "\" && runhaskell " name) (concat "* " name  " *") nil))

(defun my-launch-client (name)
  (interactive)
  (sf:async-shell-command
   (concat "cd \"" (file-name-directory (buffer-file-name))
           "\" && runhaskell AClient.hs " name " " name) (concat "*AClient " name "*") nil))

(defun my-launch-server ()
  (interactive)
  (my-runhaskell-same-buffer-file "Server"))

(defun my-launch-gtest ()
  (interactive)
  (my-runhaskell-same-buffer-file "GTestGame"))

(defun my-launch-gclient (name)
  (interactive)
  (sf:async-shell-command
   (concat "cd \"" (file-name-directory (buffer-file-name))
           "\" && runhaskell gclient.hs " name " " name) (concat "*GClient " name "*") nil))

(defun my-launch-gclients-and-server ()
  (interactive)
  (my-launch-server)
  (my-launch-gclient "GTest1")
  (my-launch-gclient "GTest2")
  )

(defun my-launch-aclients-and-server ()
  (interactive)
  (my-launch-server)
  (my-launch-client "Test1")
  (my-launch-client "Test2")
  )

(defun my-kill-gclients-and-server ()
  (interactive)
  (kill-buffer "*Server*")
  (kill-buffer "*GClient Test1*")
  (kill-buffer "*GClient Test2*")
  (kill-buffer "*GClient Test3*")
  (kill-buffer "*GClient Test4*")
  )

(defun my-atcoder-test ()
  "atcoder test run."
  (interactive)
  (let ((filename (my-get-file-name-non-extension (buffer-file-name)))
        (testdataId (read-string "Test Data Id: "))
        )
    (sf:async-shell-command
     (concat "cd \"" (file-name-directory (buffer-file-name))
             "\" && runhaskell " filename ".hs < " (concat filename testdataId) ".txt")
     (concat "*AtCoder Test " filename " *") nil)))

(defun my-hoogle ()
  (interactive)
  (browse-url (concat "http://www.haskell.org/hoogle/?hoogle=" (read-string "find:"))))

(define-key haskell-mode-map (kbd "C-c C-7") 'my-doctest-buffer-file)
(define-key haskell-mode-map (kbd "C-c C-8") 'my-run-haskell-buffer-file)
(define-key haskell-mode-map (kbd "C-c C-9") 'my-run-haskell-buffer-file-with-args)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'ghc-browse-document)
(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-show-type)
(define-key haskell-mode-map (kbd "C-c C-s") 'ghc-save-buffer)
(define-key haskell-mode-map (kbd "C-x C-d") 'dired)

(define-key haskell-mode-map (kbd "C-c C-^") 'my-atcoder-test)
(define-key haskell-mode-map (kbd "C-c C-0") 'my-hoogle)

(define-key haskell-mode-map (kbd "C-c C-o") 'ghc-complete)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info)
(define-key haskell-mode-map (kbd "C-c C-d") 'anything-ghc-browse-document)
(define-key haskell-mode-map (kbd "C-c C-t") 'ghc-insert-template)
(define-key haskell-mode-map (kbd "C-c C-s") 'ghc-save-buffer)
(define-key haskell-mode-map (kbd "C-c C-g") 'my-launch-gclients-and-server)
(define-key haskell-mode-map (kbd "C-c C-k") 'my-kill-gclients-and-server)
(define-key haskell-mode-map (kbd "C-c C-y") 'my-launch-aclients-and-server)
(define-key haskell-mode-map (kbd "C-c C-b") 'my-launch-gtest)
(define-key haskell-mode-map (kbd "C-m")     'my-backward-word)
(define-key haskell-mode-map (kbd "C-c C-n") 'flymake-goto-next-error)
(define-key haskell-mode-map (kbd "C-c C-p") 'flymake-goto-prev-error)
(define-key haskell-mode-map (kbd "C-c C-u") 'ghc-flymake-display-errors)

(define-key haskell-mode-map (kbd "C-c C-a") '(lambda () (interactive) (insert " <*> ")))
(define-key haskell-mode-map (kbd "C-c C-f") '(lambda () (interactive) (insert " <$> ")))
(define-key haskell-mode-map (kbd "C-c C-;") '(lambda () (interactive) (insert " = ")))
(define-key haskell-mode-map (kbd "C-c C-l") '(lambda () (interactive) (insert " <- ")))
(define-key haskell-mode-map (kbd "C-c C-6") '(lambda () (interactive) (insert " .&. ")))
(define-key haskell-mode-map (kbd "C-c C-\\") '(lambda () (interactive) (insert " .|. ")))
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
(define-key inferior-haskell-mode-map (kbd "C-x C-k") 'my-inferior-haskell-kill-buffer)

(defun my-shell-command-on-current-directory (cmd)
  ""
  (concat "cd \"" (my-get-buffer-directory)
          "\" && " cmd))

(defun my-inferior-haskell-load-file ()
  "load hs and hlint."
  (interactive)

  (sf:async-shell-command
   (my-shell-command-on-current-directory
    (concat "hlint " (buffer-file-name))) "*hs-lint*")

  (inferior-haskell-load-file)
  )

(defun my-inferior-haskell-kill-buffer ()
  "kill buffer with delete ghc process."
  (interactive)
  (comint-send-eof)
  (kill-buffer (current-buffer))
  )


(defun my-haskell-wall ()
  "wall build"
  (interactive)
  (sf:async-shell-command
   (my-shell-command-on-current-directory
    (concat "ghc -Wall " (buffer-file-name))) "*hs-wall*"))

(defun my-hs-save-buffer ()
  "wall build"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-unix t)
  (delete-trailing-whitespace)
  (haskell-mode-save-buffer)
  (sf:async-shell-command
   (my-shell-command-on-current-directory
    (concat "ghc -Wall " (buffer-file-name))) "*hs-wall*"))

(define-key haskell-mode-map (kbd "C-c C-e") 'inferior-haskell-load-and-run)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)
(define-key haskell-mode-map (kbd "C-c s") 'hs-scan)
(define-key haskell-mode-map (kbd "C-o") 'auto-complete)

(define-key haskell-indentation-mode-map (kbd "C-j") 'haskell-newline-and-indent)
(define-key haskell-indentation-mode-map (kbd "C-m") my-backward-word-command)

(define-key inferior-haskell-mode-map (kbd "C-m") my-backward-word-command)
(define-key inferior-haskell-mode-map (kbd "C-j") 'comint-send-input)
(define-key inferior-haskell-mode-map (kbd "C-c C-h") 'sf:hoogle)

(define-key haskell-mode-map (kbd "C-c C-e") 'my-launch-client)
(define-key haskell-mode-map (kbd "C-c C-s") 'my-launch-server)
(define-key haskell-mode-map (kbd "C-c C-SPC") 'my-haskell-wall)

(define-key haskell-mode-map (kbd "C-m") my-backward-word-command)
(define-key haskell-mode-map (kbd "C-c C-h") 'sf:hoogle)

(my-require 'bm)
(define-key haskell-mode-map (kbd "M-C-m") 'bm-toggle)

;; ;; Customization
;; (custom-set-variables
;;  '(haskell-tags-on-save nil)
;;  '(haskell-stylish-on-save nil))

(provide 'init-haskell)
