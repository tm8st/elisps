;;; init-vc.el --- version control setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, vc
;; creation time: Sat May  8 22:32:11 2010
;;; Commentary:

;;; Code:

;;;-------------------------------
;;; vc
;;;-------------------------------
;; (setq vc-default-back-end 'Git)

;; C-x v v     vc-next-action -- perform the next logical control operation on file
;; C-x C-q    by default, C-x C-q is no longer bound, so it's better to use the above binding)
;; C-x v i     vc-register -- add a new file to version control

;; C-x v ~     vc-version-other-window -- look at other revisions
;; C-x v =     vc-diff -- diff with other revisions
;; C-x v u     vc-revert-buffer -- undo checkout
;; C-x v c     vc-cancel-version -- delete the latest revision (often it makes more sense to look at an old revision and check that in again!)
;; C-x v d     vc-directory -- show all files which are not up to date
;; C-x v g     vc-annotate -- show when each line in a cvs file was added and by whom
;; C-x v s     vc-create-snapshot -- tag all the files with a symbolic name
;; C-x v r     vc-retrieve-snapshot -- undo checkouts and return to a snapshot with a symbolic name
;; C-x v l     vc-print-log -- show log (not in ChangeLog format)
;; C-x v a     vc-update-change-log -- update ChangeLog
;; C-x v m     vc-merge
;; C-x v h     vc-insert-headers
;; M-x vc-resolve-conflicts -- pop up an ediff-merge session on a file with conflict markers

(provide 'init-vcs)
