;;; subdirs.el --- subdir add loadpath utl

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: subdir, loadpath
;; creation time: Thu Apr 29 22:50:38 2010
;;; Commentary:

;;; Code:

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (normal-top-level-add-subdirs-to-load-path))