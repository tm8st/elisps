;;; init-linum.el --- linum setting

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, linum
;; creation time: Tue May  4 21:29:54 2010
;;; Commentary:

;; 桁数、フォントによって数字が読めなくなるのを回避

;;; Code:
(require 'linum)
;; (require 'linum+)
(global-linum-mode t)
(set-face-background 'linum "black")
(set-face-foreground 'linum "bisque1")

(defvar my-linum-min-windth 2)

(defface my-linum-current-line
  '((t :inherit (shadow default))
	(((class color)
	  (foreground light)) (:foreground "Green" :background "gray15")))
  "Face for displaying line numbers in the display margin."
  :group 'linum)

;; (set-face-foreground 'linum "gray60")
(set-face-foreground 'my-linum-current-line "gray60")
(set-face-background 'my-linum-current-line "gray20")

(defvar linum-current-buffer nil "update current buffer.")

(defun linum-update (buffer)
  "Update line numbers for all windows displaying BUFFER."
  (with-current-buffer buffer
	(when linum-mode
	  (setq linum-available linum-overlays)
	  (setq linum-overlays nil)
	  (setq linum-current-buffer buffer)
	  (save-excursion
		(mapc #'linum-update-window
			  (get-buffer-window-list buffer nil 'visible)))
	  (mapc #'delete-overlay linum-available)
	  (setq linum-available nil))))

;; フォーマットと幅を微調整
(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let (
	(line (line-number-at-pos))
	(limit (window-end win t))
	(fmt (cond ((stringp linum-format) linum-format)
		   ((eq linum-format 'dynamic)
		    (let ((w (max my-linum-min-windth (length (number-to-string
							       (count-lines (point-min) (point-max)))))))
		      (concat "%0" (number-to-string w) "d")))))
	(width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (string= (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delete o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      (forward-line)
      (setq line (1+ line)))
    (set-window-margins win (+ width 1))))

(provide 'init-linum)