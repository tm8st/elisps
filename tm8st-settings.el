;;; tm8st-settings.el --- tm8st emacs-settings package

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: emacs-settings
;; creation time: Wed Aug 11 20:56:00 2010
;;; Commentary:

;;; Code:

("http://github.com/tm8st/elisps/raw/master/tm8st-settings.el"
 (tm8st-all-settings
  virtual nil
  "this is a virtual package to build garaemon's emacs environment"
  (tm8st-anything-settings))

 (tm8st-global-settings
  library
  (cvs :pserver:anonymous@cvs.savannah.gnu.org:/sources/global global)
  "global is tags util."
  nil
  )

 ;; (tm8st-anything-settings
 ;;  library
 ;;  (http://www.emacswiki.org/emacs/download/anything-gtags.el
 ;;   http://www.emacswiki.org/emacs/download/anything-etags.el
 ;;   http://www.emacswiki.org/emacs/download/anything-extension.el
 ;;   http://www.emacswiki.org/emacs/download/anything-kyr.el
 ;;   http://www.emacswiki.org/emacs/download/anything-c-moccur
 ;;   http://www.emacswiki.org/emacs/download/anything-project
 ;;   http://www.emacswiki.org/emacs/download/anything-howm)
 ;;  "settings for anything."
 ;;  (tm8st-global-settings) ;依存してる他のパッケージ
 ;;  (:byte-compile))
 )
