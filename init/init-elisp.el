;;; init-elisp.el --- elisp

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, elisp
;; creation time: Wed Apr 28 00:07:49 2010
;;; Commentary:

;;; Code:

(my-require 'init-compile-env)
(my-require 'auto-install)
(my-require 'highlight-parentheses)

(defun my-emacs-lisp-mode-hook ()
  ;; (linum-mode t)
  (hl-line-mode t)
  (yalinum-mode t)
  (when use-gui-setting
    (highlight-parentheses-mode)
    )
  (easy-imenu-index-generator-set-for-current-buffer easy-imenu-index-generator-setting-elisp-sample)
  )

(setq auto-mode-alist
	  (append
	   '(("\\.el$" . emacs-lisp-mode)
		 )
	   auto-mode-alist))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

(define-key emacs-lisp-mode-map (kbd "C-q C-e") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-l C-e") 'my-eval-buffer-or-region)

;;;-------------------------------
;;; auto-install misc
;;;-------------------------------
(defvar my-auto-install-requires-list nil
  "auto install elisps.")

(defun my-auto-install-from-requires ()
  "Batch install many packages form elisp my-requires."
  (interactive)
  (setq my-auto-install-requires-list nil)
  (dolist (req (split-string (shell-command-to-string "grep 'my-require ' *.el") "\n"))
    (when (eq (string-match ":+\\( \t\\)*;" req) nil)
      (progn
        (let ((begin (string-match "'" req)))
          (if (eq begin nil) nil
            (let ((end (string-match "\\()\\| \\|\n\\|\t\\)" req begin)))
              (if (eq end nil) nil
                (progn
                  (let ((name (substring req (+ 1 begin) end)))
                    ;; (setq name (concat "http://www.emacswiki.org/emacs/download/" name ".el"))
                    (unless (member
                             (concat name ".el")
                             init-load-elisp-list)
                      (progn
                        (unless (member name my-auto-install-requires-list)
                          (add-to-list 'my-auto-install-requires-list name))
                        (sleep-for 5)
                        ;; (auto-install-from-emacswiki name)
                        (auto-install-from-url name)))))))))))))

(defvar my-auto-update-elisp-list
  (list
   "anything"
   "anything-auto-install"
   "anything-c-moccur"
   "anything-complete"
   "anything-config"
   "anything-extension"
   "anything-grep"
   "anything-gtags"
   "anything-howm"
   "anything-kyr"
   "anything-kyr-config"
   "anything-project"
   "auto-async-byte-compile"
   "auto-install"
   "browse-kill-ring"
   "color-moccur"
   "multi-shell"
   "sequential-command"
   "sequential-command-config"
   "shell-pop"
   "text-translator"
   ))

(defun my-auto-update-elisps ()
  "update elisp from emacswiki."
  (interactive)
  (customize-set-value 'auto-install-save-confirm nil)
  (dolist (elisp my-auto-update-elisp-list)
    (sleep-for 3)
    (auto-install-from-emacswiki (concat elisp ".el"))
    ))

;; (defun my-auto-async-byte-compile-display-function (buf)
;;   "ignore."
;;   (interactive)
;;   (display-message-or-buffer buf)
;;   )

;; (customize-set-value 'auto-async-byte-compile-display-function 'my-auto-async-byte-compile-display-function)

;; (auto-install-from-url "http://nschum.de/src/emacs/highlight-parentheses/highlight-parentheses.el")
;; (auto-install-from-url "http://nschum.de/src/emacs/window-numbering-mode/window-numbering.el")

;; git repo
;; (auto-install-from-url "http://github.com/imakado/emacs-smartchr/raw/539c8570fdf9b6007136db668856a4563b90afe3/smartchr.el")
;; "highlight-parentheses"
;; http://nschum.de/src/emacs/highlight-parentheses/highlight-parentheses.el
;; "smartchr"
;; http://github.com/imakado/emacs-smartchr/blob/master/smartchr.el
;; "window-numbering"
;; http://nschum.de/src/emacs/window-numbering-mode/window-numbering.el

;;;-------------------------------
;;; eldoc lisp
;;;-------------------------------
(my-require 'eldoc)
;; (install-elisp-from-emacswiki "eldoc-extension.el")
;; (my-require 'eldoc-extension)
(setq eldoc-idle-delay 0.15)
(setq eldoc-echo-area-use-multiline-p t)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Lisp用にSLIMEの設定

;; lisp-mode
(setq inferior-lisp-program "clisp")    ;; clisp用
;; (setq inferior-lisp-program "sbcl")  ;; sbcl用

(when (my-require 'slime)
  (slime-setup))

(customize-set-variable 'eldoc-minor-mode-string (purecopy " ED"))

(defun my-scratch ()
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scratch*"))
  (lisp-interaction-mode)
  (goto-char (point-min))
  (when (looking-at ";")
    (forward-line 4)
    (delete-region (point-min) (point)))
  (goto-char (point-max)))

;; From sean @ http://emacsblog.org/2007/10/07/declaring-emacs-bankruptcy/
(add-hook 'after-save-hook 'my-recompile-el)
(setq byte-compile-verbose t)

(defun my-recompile-el ()
  (interactive)
  (save-excursion
    (when (and buffer-file-name
               (unless (string-match "/junk/"  buffer-file-name) 0)
               (string-match "/.*\\.el$"  buffer-file-name)
               (file-newer-than-file-p buffer-file-name
                                       (concat buffer-file-name "c")))
      ;; (let ((byte-compile-warnings '()))
      ;; (let ((byte-compile-warnings '(not free-vars)))
      (byte-compile-file buffer-file-name nil))))

;; http://sheephead.homelinux.org/2011/06/17/6724/
(require 'package)

;;リポジトリにMarmaladeを追加
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;インストールするディレクトリを指定
(setq package-user-dir (concat user-emacs-directory "vendor/elpa"))
;;インストールしたパッケージにロードパスを通してロードする
(package-initialize)

(my-require 'open-junk-file)
(my-require 'lispxmp)
(global-set-key (kbd "C-x C-z") 'open-junk-file)
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)

(provide 'init-elisp)
