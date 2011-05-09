;;; init-R.el --- R setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, R

;;; Commentary:

;;; Code:

(require 'ess)
(require 'ess-comp)
(require 'ess-r-d)
(setq auto-mode-alist
     (cons (cons "\\.r$" 'R-mode) auto-mode-alist))

(require 'auto-complete-acr)
(add-to-list 'ac-modes 'R-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;anything-c-source-R-helpの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-help
      '((name . "R objects / help")
				(init . (lambda ()
        ;; this grabs the process name associated with the buffer
			  (setq anything-c-ess-local-process-name ess-local-process-name)))
				(candidates . (lambda ()
												(condition-case nil
														(ess-get-object-list anything-c-ess-local-process-name)
			  (error nil))))
	(action
	 ("help" . ess-display-help-on-object)
	 ("head (10)" . (lambda(obj-name)
			  (ess-execute (concat "head(" obj-name ", n = 10)\n") nil (concat "R head: " obj-name))))
	 ("head (100)" . (lambda(obj-name)
			   (ess-execute (concat "head(" obj-name ", n = 100)\n") nil (concat "R head: " obj-name))))
	 ("tail" . (lambda(obj-name)
		     (ess-execute (concat "tail(" obj-name ", n = 10)\n") nil (concat "R tail: " obj-name))))
	 ("str" . (lambda(obj-name)
		    (ess-execute (concat "str(" obj-name ")\n") nil (concat "R str: " obj-name))))
	 ("summary" . (lambda(obj-name)
			(ess-execute (concat "summary(" obj-name ")\n") nil (concat "R summary: " obj-name))))
	 ("view source" . (lambda(obj-name)
			    (ess-execute (concat "print(" obj-name ")\n") nil (concat "R object: " obj-name))))
	 ("dput" . (lambda(obj-name)
		     (ess-execute (concat "dput(" obj-name ")\n") nil (concat "R dput: " obj-name)))))
	(volatile)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; anything-c-source-R-localの設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq anything-c-source-R-local
      '((name . "R local objects")
	(init . (lambda ()
		  ;; this grabs the process name associated with the buffer
		  (setq anything-c-ess-local-process-name ess-local-process-name)
		  ;; this grabs the buffer for later use
		  (setq anything-c-ess-buffer (current-buffer))))
	(candidates . (lambda ()
			(let (buf)
			  (condition-case nil
			      (with-temp-buffer
				(progn
				  (setq buf (current-buffer))
				  (with-current-buffer anything-c-ess-buffer
				    (ess-command "print(ls.str(), max.level=0)\n" buf))
				  (split-string (buffer-string) "\n" t)))
			    (error nil)))))
	(display-to-real . (lambda (obj-name) (car (split-string obj-name " : " t))))
	(action
	 ("str" . (lambda(obj-name)
		    (ess-execute (concat "str(" obj-name ")\n") nil (concat "R str: " obj-name))))
	 ("summary" . (lambda(obj-name)
			(ess-execute (concat "summary(" obj-name ")\n") nil (concat "R summary: " obj-name))))
	 ("head (10)" . (lambda(obj-name)
			  (ess-execute (concat "head(" obj-name ", n = 10)\n") nil (concat "R head: " obj-name))))
	 ("head (100)" . (lambda(obj-name)
			   (ess-execute (concat "head(" obj-name ", n = 100)\n") nil (concat "R head: " obj-name))))
	 ("tail" . (lambda(obj-name)
		     (ess-execute (concat "tail(" obj-name ", n = 10)\n") nil (concat "R tail: " obj-name))))
	 ("print" . (lambda(obj-name)
		      (ess-execute (concat "print(" obj-name ")\n") nil (concat "R object: " obj-name))))
	 ("dput" . (lambda(obj-name)
		     (ess-execute (concat "dput(" obj-name ")\n") nil (concat "R dput: " obj-name)))))
	(volatile)))

;; anything.elの設定
(require 'anything-config)
(define-key global-map (kbd "C-x b") 'anything)

(setq anything-sources
      '(anything-c-source-buffers
	anything-c-source-emacs-commands
	anything-c-source-file-name-history
	anything-c-source-locate
	anything-c-source-man-pages
	anything-c-source-occur
	anything-c-source-recentf
	anything-c-source-R-local
	anything-c-source-R-help
	))

(provide 'init-R)

