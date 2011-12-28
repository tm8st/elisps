;;; init-uesript.el --- unreal script setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, unreal script
;; creation time: Wed Apr 28 00:55:31 2010
;;; Commentary:

;;; Code:

(defun my-ue3-read-gamename ()
  (completing-read "Game Name:" my-ue3-gamenames nil t))

(defun my-ue3-make-script ()
 ""
  (interactive)
  (message (concat (gtags-get-rootpath) " make."))
  (async-shell-command
   (concat (gtags-get-rootpath) "../../Binaries/Win32/" (my-ue3-read-gamename) "Game.com make")
   "*ue3 make script*"))

(defun my-ue3-full-make-script ()
 ""
  (interactive)
  (message (concat (gtags-get-rootpath) " full make."))
  (async-shell-command
   (concat (gtags-get-rootpath) "../../Binaries/Win32/" (my-ue3-read-gamename) "Game.com make -full")
   "*ue3 full make script*"))

(defun my-ue3-full-make-script-64 ()
 ""
  (interactive)
  (message (concat (gtags-get-rootpath) " full make."))
  (async-shell-command
   (concat (gtags-get-rootpath)
           "../../Binaries/Win64/" (my-ue3-read-gamename) "Game.com make -full")
   "*ue3 full make script*"))

(defun my-ue3-launch-editor ()
 ""
  (interactive)
  (start-process-shell-command
   "*UE3 Launch Editor*"
   "*UE3 Launch Editor*"
   (concat (gtags-get-rootpath)
           "../../Binaries/Win32/" (my-ue3-read-gamename) "Game.exe Editor")))

(defun my-ue3-launch ()
 ""
  (interactive)
  (start-process-shell-command
   "*UE3 Launch*"
   "*UE3 Launch*"
   (concat (gtags-get-rootpath)
           "../../Binaries/Win32/" (my-ue3-read-gamename) "Game.exe")))

(global-set-key (kbd "C-l C-u C-m") `my-ue3-make-script)
(global-set-key (kbd "C-l C-u C-f") `my-ue3-full-make-script)
(global-set-key (kbd "C-l C-u C-d") `my-ue3-full-make-script-64)
(global-set-key (kbd "C-l C-u C-l") `my-ue3-launch)
(global-set-key (kbd "C-l C-u C-l") `my-ue3-launch-editor)

(provide 'init-uescript)
