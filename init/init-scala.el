;;; init-scala.el --- scala setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, scala
;; creation time: Thu May  6 00:37:26 2010
;;; Commentary:

;;; Code:

(my-require 'auto-complete)
(my-require 'highlight-parentheses)
(my-require 'init-keybindings)

;; scala-mode
(unless my-initialized
  ;; (add-to-list 'load-path "~/Softwares/scala-2.8.0.RC3/misc/scala-tool-support/emacs")
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
  )
(my-require 'scala-mode)
(my-require 'scala-mode-auto)
(modify-coding-system-alist 'file "\\.scala$" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-to-list 'ac-modes 'scala-mode)

;;;-------------------------------
;;; ensime
;;;-------------------------------
;; (add-to-list 'load-path "ENSIME_ROOT/elisp/")
;; (add-to-list 'load-path "/Users/mys/elisps/external/ensime/src/main/elisp/")
;; (my-require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
;; (customize-set-value 'ensime-default-server-cmd "/Users/mys/elisps/external/ensime/etc/scripts/server.sh")

(defun my-run-scala-buffer ()
  (interactive)
  (async-shell-command
   (concat "scala " (buffer-file-name (current-buffer)))))

(my-require 'easy-imenu-index-generator-config)

(defun my-scala-mode-hook ()
  ;; (auto-complete-mode t)
  (easy-imenu-index-generator-set-for-current-buffer easy-imenu-index-generator-scala)
  (hl-line-mode t)
  (highlight-parentheses-mode t)
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  )

;; (my-require 'sbt)

(add-hook 'scala-mode-hook 'my-scala-mode-hook)

(define-key scala-mode-map (kbd "C-c C-c")
  `my-run-scala-buffer)
(define-key scala-mode-map (kbd "C-m")
  `my-backward-word)
(define-key scala-mode-map (kbd "C-t")
  `my-forward-word)

(when (my-is-mac)
  ;; (my-require 'ensime)
  ;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

  ;; MINI HOWTO: open .scala file. Ensure bin/server.sh is executable. M-x ensime

  ;; (my-require 'ensime)
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

(defun android-search ()
  (interactive)
  (let ((w (word-at-point)))
    (if w
	(browse-url
	 (concat "http://developer.android.com/search.html#q="
		 (url-hexify-string w)
		 "&t=0"))
      (error "no word at point"))))

;; (global-set-key (kbd "C-l C-s C-g") 'android-search)

(provide 'init-scala)
