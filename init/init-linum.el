;;; init-linum.el --- linum setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, linum
;; creation time: Tue May  4 21:29:54 2010
;;; Commentary:

;;; Code:

(require 'linum)
;; (require 'linum+)
(global-linum-mode t)

;; (defun my-linum-format (line)
;;   (let ((w (length (number-to-string
;; 		    (count-lines (point-min) (point-max))))))
;;     (propertize (format (concat "%" (number-to-string w) "d"))
;; 		'face (list
;; 		':background (concat "gray" (number-to-string (/ line count-line)))))))

;;(customize-set-variable 'linum-format 'my-linum-format)
(customize-set-variable 'linum-format 'dynamic)

(provide 'init-linum)

