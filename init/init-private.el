;;; init-private.el --- private setting file

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, private
;; creation time: Thu Apr 29 22:32:33 2010
;;; Commentary:

;;; Code:

;;;----------------------------------------
;;; user
;;;----------------------------------------
(defvar user-full-name "???")
(defvar user-mail-address "???")

;;;----------------------------------------
;;; twitter
;;;----------------------------------------
(defvar my-twittering-username "???")
(defvar my-twittering-password "???")

;;;-------------------------------
;;; hatena
;;;-------------------------------
(defvar my-hatena-usrid "???")

;;;-------------------------------
;;; ファイルキャッシュ
;;;-------------------------------
(defvar file-cache-path
  (list
   ))

;;;-------------------------------
;;; よく使うディレクトリ
;;;-------------------------------
(defvar develop-path nil)
(defvar etc-path nil)

;;;-------------------------------
;;; お気に入りディレクトリ
;;;-------------------------------
(defvar favolite-directory-assoc-template-list nil)

;;;-------------------------------
;;; yasnippet directory
;;;-------------------------------
(defvar my-yas/load-directory "???")

;;;-------------------------------
;;; rsense
;;;-------------------------------
(defvar my-rsense-home "???")

;;;-------------------------------
;;; popup template
;;;-------------------------------
(defvar my-mail-sentence-template-list nil)
(defvar my-build-sentence-assoc-template-list nil)
(defvar my-directory-sentence-assoc-template-list (list ))

(when (my-is-mac)
  (add-to-list 'my-directory-sentence-assoc-template-list
	       (list
		`("Download" "~/Downloads/")
		`("Picture" "~/Pictures/")
		`("Document" "~/Documents/")
		)))
(when (my-is-windows))

(provide 'init-private)