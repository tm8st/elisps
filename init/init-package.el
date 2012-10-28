;;; init-package.el --- elisp package management.

;; Copyright (C) 2012 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, packaged

;;; Commentary:

(my-require 'package)

;;リポジトリにMarmaladeを追加
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
;;インストールするディレクトリを指定
(setq package-user-dir (concat my-dropbox-directory "Emacs/packages"))
;;インストールしたパッケージにロードパスを通してロードする
(package-initialize)

(provide 'init-package)
