;;; init-linum.el --- linum setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, linum
;; creation time: Tue May  4 21:29:54 2010
;;; Commentary:

;;; Code:

(require 'yalinum)
(global-yalinum-mode t)

(when (my-is-mac)
  (setq yalinum-width-base 1)
  (setq yalinum-width-scale 0.5)
  (setq yalinum-line-number-display-format " %0$numd")
  )
(when (my-is-windows)
  (setq yalinum-width-base 0)
  (setq yalinum-width-scale 1)
  (setq yalinum-line-number-display-format "%0$numd")
  )

(provide 'init-linum)
