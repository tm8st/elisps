;;; init-scala.el --- scala setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, scala
;; creation time: Thu May  6 00:37:26 2010
;;; Commentary:

;;; Code:

(require 'scala-imenu)
(require 'auto-complete)
(require 'highlight-parentheses)

;; scala-mode
(unless my-initialized
  ;; (add-to-list 'load-path "~/Softwares/scala-2.8.0.RC3/misc/scala-tool-support/emacs")
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
  )
(require 'scala-mode nil t)
(require 'scala-mode-auto nil t)
(modify-coding-system-alist 'file "\\.scala$" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;;;-------------------------------
;;; ensime
;;;-------------------------------
;; (add-to-list 'load-path "ENSIME_ROOT/elisp/")
;; (add-to-list 'load-path "/Users/mys/elisps/external/ensime/src/main/elisp/")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (customize-set-value 'ensime-default-server-cmd "/Users/mys/elisps/external/ensime/etc/scripts/server.sh")

(defun my-run-scala-buffer ()
  (interactive)
  (async-shell-command
   (concat "scala " (buffer-file-name (current-buffer)))))

(defun my-scala-mode-hook ()
  ;; (auto-complete-mode t)
  (scala-imenu-set-for-current-buffer)
  (hl-line-mode t)
  (highlight-parentheses-mode t)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  )

;; (require 'sbt)

(add-hook 'scala-mode-hook 'my-scala-mode-hook)

(define-key scala-mode-map (kbd "C-c C-c")
  `my-run-scala-buffer)
(define-key scala-mode-map (kbd "C-m")
  `my-backward-word)
(define-key scala-mode-map (kbd "C-t")
  `my-forward-word)

(when (my-is-mac)
  ;; (require 'ensime)
  ;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; MINI HOWTO: open .scala file. Ensure bin/server.sh is executable. M-x ensime

  ;; (require 'ensime)
  ;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; TAB - Start completing a method/variable.
  ;; C-c t / Double-Click - Inspect the type of the expression under the cursor.
  ;; M-. / Control-Click - Jump to definition of symbol under cursor.
  ;; M-, - Pop back to previously visited position.
  ;; Double-Click(on an import statement) - Inspect the package under cursor.
  ;; Mouse Hover - Echo the type of the expression under the cursor.
  ;; C-c p - Inspect the package of the current source file.
  ;; C-c o - Inspect the package specified in .ensime as :project-package.
  ;; . - Forward one page in the inspector history.
  ;; , - Backward one page in the inspector history.
  ;; M-n / TAB - Forward one link in the inspector.
  ;; M-p - Backward one link in the inspector.
  ;; C-c C-a - Switch to the sbt command-line (works for sbt projects only)
  ;; C-c C-z - Switch to the scala interpreter, with project classes in the classpath.
  ;; C-c c - Type-check the current file.
  ;; C-c a - Type-check all files in the project.
  )

(provide 'init-scala)
