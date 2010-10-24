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
;;; ディレクトリ指定
;;;-------------------------------
(defvar my-develop-path nil)
(defvar my-etc-path nil)
(defvar my-exec-path nil)

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
(defvar my-build-command-assoc-template-list nil)
(defvar my-directory-name-assoc-template-list (list ))

(unless my-initialized
  (progn
    (when (my-is-mac)
      (add-to-list 'my-directory-name-assoc-template-list
		   (list
		    `("Download" "~/Downloads/")
		    `("Picture" "~/Pictures/")
		    `("Document" "~/Documents/")
		    )))

    (when (my-is-windows))
    ))

(defvar my-source-file-extention-list `("scala" "inl" "uci" "cpp" "c" "h" "uc" "usf" "lisp" "el" "pl" "rb"))
(defvar my-doc-file-extention-list `("howm" "org" "txt" "pdf" "ppt"))
(defvar my-music-file-extention-list `("m4a" "mp3" "wav"))
(defvar my-exe-file-extention-list `("air" "exe" "app"))
(defvar my-archive-file-extention-list `("lzh" "tar" "tar.gz" "tgz" "tar.bz2" "dmg" "pkg" "jar" "zip" "tar"))

(provide 'init-private)
