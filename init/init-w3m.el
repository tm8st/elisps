;;; init-w3m.el --- w3m inii

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, w3m, browser
;; creation time: Sun Jul  4 23:16:34 2010
;;; Commentary:

;;; Code:

;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot login
;; CVS password:
;; # パスワードは設定されていません．単に Enter/Return キーを押して下さい．
;; % cvs -d :pserver:anonymous@cvs.namazu.org:/storage/cvsroot co emacs-w3m

(require 'w3m)

;; (require 'ldr)
;; (require 'ldr-w3m)
;; (setq ldr-html-render-function 'ldr-html-w3m-render)
;; (setq ldr-html-image-display-function 'ldr-html-w3m-display-image)

;; 使いたい外部ブラウザを browse-url へと登録していないならば、
;; 合わせて登録しておくと良いでしょう。
;; (setq browse-url-generic-program "/usr/bin/firefox")

(provide 'init-w3m)

