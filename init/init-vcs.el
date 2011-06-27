 ;;; init-vc.el --- version control setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, vc
;; creation time: Sat May  8 22:32:11 2010
;;; Commentary:

;;; Code:

(require 'vc)
(require 'vc-dir)
(require 'diff)
(require 'ediff)

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
 	
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-multiframe nil)
(setq-default ediff-auto-refine-limit 10000)

(define-key vc-dir-mode-map "d" 'vc-diff)
(define-key vc-dir-mode-map "r" 'vc-revert)

;; (add-hook 'diff-mode-hook
;;           (lambda ()
;;             (when use-gui-setting
;;               (set-face-foreground 'diff-context-face "grey50")
;;               (set-face-background 'diff-header-face "black")
;;               (set-face-underline-p 'diff-header-face t)
;;               (set-face-foreground 'diff-file-header-face "MediumSeaGreen")
;;               (set-face-background 'diff-file-header-face "black")
;;               (set-face-foreground 'diff-index-face "MediumSeaGreen")
;;               (set-face-background 'diff-index-face "black")
;;               (set-face-foreground 'diff-hunk-header-face "plum")
;;               (set-face-background 'diff-hunk-header-face"black")
;;               (set-face-foreground 'diff-removed-face "pink")
;;               (set-face-background 'diff-removed-face "gray5")
;;               (set-face-foreground 'diff-added-face "light green")
;;               (set-face-foreground 'diff-added-face "white")
;;               (set-face-background 'diff-added-face "SaddleBrown")
;;               (set-face-foreground 'diff-changed-face "DeepSkyBlue1"))))

(add-hook 'diff-mode-hook
          (lambda()
            (define-key diff-mode-map (kbd "C-M-n") 'diff-file-next)
            (define-key diff-mode-map (kbd "C-M-p") 'diff-file-prev)
            (define-key diff-mode-map (kbd "M-k") 'diff-hunk-kill)
            (define-key diff-mode-map (kbd "C-M-k") 'diff-file-kill)))

(provide 'init-vcs)
