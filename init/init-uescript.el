
;;; init-uesript.el --- unreal script setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, unreal script
;; creation time: Wed Apr 28 00:55:31 2010
;;; Commentary:

;;; Code:

;; (defvar uescript-symbol-regex "native?[ \t]function ")
;; (defvar uescript-varialbe-symbol-regex "^var")

;; (defun uescript-imenu-create-index ()
;;   (let (index)
;;     (goto-char (point-min))
;;     (while (re-search-forward uescript-symbol-regex (point-max) t)
;;       (push (cons (match-string 1) (match-beginning 1)) index))
;;     (nreverse index)))
;; (setq imenu-create-index-function 'uescript-imenu-create-index)
;; (add-hook 'html-mode (lambda () (setq imenu-create-index-function 'html-imenu-create-index)))
;; (setq which-func-modes (append which-func-modes '(html-mode)))


(defvar identifier-regexp "[a-zA-Z0-9.$_]+")
(defvar function-regex (concat "native?[ \t]function " identifier-regexp))
(defvar varialbe-symbol-regex "^var")

;; var() bool bDelayFullOn; // Delay then go full-on. 
;; var actor Trigger;
 
;; function BeginPlay();
;; native function BeginPlay()

;; (defun uescript-imenu-create-index ()
;;   (save-excursion)
;;   (let (index)
;;     (goto-char (point-min))
;;     (while (re-search-forward "^function" (point-max) t)
;;       (push (cons (match-string 1) (match-beginning 1)) index)
;;       (message (match-string 1))
;;       )
;;     (nreverse index)))
;; (setq imenu-create-index-function 'uescript-imenu-create-index)

(provide 'init-uescript)
