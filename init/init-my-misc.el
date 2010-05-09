;;; init-my-misc.el --- my misc

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, misc
;; creation time: Thu Apr 29 21:41:11 2010
;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))

;;;-------------------------------
;;; original-move-functions
;;;-------------------------------
(defun my-forward-word ()
  "カーソル位置から一単語前進"
  (interactive)
  (progn
	(let ((char (char-syntax (following-char))))
	  (cond
	   ((= char ?\ ) (skip-chars-forward " \t"))
	   ((= char ?-) (skip-chars-forward " \t"))
	   ((= char ?<) (skip-chars-forward ";/"))
	   ((= char ?_) (forward-word))
	   ((= char ?w) (forward-word))
  	   ((= char ?/) (forward-word))
	   (t (forward-char))
	   )
	  )
    ))

(defun my-backward-word ()
  "カーソル位置から一単語後退"
  (interactive)
  (progn
	(let ((char (char-syntax (following-char))))
	  (cond
	   ((= char ?\ ) (skip-chars-backward "a-zA-Z \t"))
	   ((= char ?-) (skip-chars-backward "a-zA-Z \t"))
	   ((= char ?<) (skip-chars-backward ";/"))
	   ((= char ?_) (backward-word))
	   ((= char ?w) (backward-word))
  	   ((= char ?/) (backward-word))
	   ((= char ?)) (backward-word))
	   (t (backward-char))
	   )
	  )
    ))

(defun my-scroll-up ()
  "固定幅スクロール 通常との違いはちゃんと一番上まで行くこと"
  (interactive)
  (progn
    (next-line (- (window-height) 4))
    ))

(defun my-scroll-down ()
  "固定幅スクロール 通常との違いはちゃんと一番上まで行くこと"
  (interactive)
  (progn
    (previous-line (- (window-height) 4))
    ))

(defun my-replace-string ()
  "カーソル位置にある文字を次に入力する文字で置換"
  (interactive)
  (if mark-active
      (let* ((start (mark)) (end (point)) (input (read-string (concat "replace [ " (buffer-substring start end) " ] to:"))))
	(kill-region start end)
	(insert input))
    (progn
      (message (concat "replace [ " (string (char-after)) " ] to:"))
      (let ((input (read-char)))
	(delete-char 1)
	(insert input)))))
(defvar my-changecase-word-state 0)

(defun my-changecase-word (cnt)
  "カーソルのすぐ左にある単語を大文字→先頭だけ大文字→小文字にする。"
  (interactive "p")
  (if (not (eq last-command 'my-changecase-word))
	 (setq my-changecase-word-state 0))
  (cond ((= my-changecase-word-state 0)
	  (upcase-word (- cnt))
	  (setq my-changecase-word-state 1))
	 ((= my-changecase-word-state 1)
	  (capitalize-word (- cnt))
	  (setq my-changecase-word-state 2))
	 (t
	  (downcase-word (- cnt))
	  (setq my-changecase-word-state 0))))

;;;-------------------------------
;;; delete | kill | copy functions
;;;-------------------------------
(defun my-kill-region (arg)
  "C-u を何回押したかでコマンド分岐"
  (interactive "P")
  (cond
   ((equal arg '(16)) (my-delete-region-or-follow-kill-word)) ;;C-u C-u
   ((equal arg '(4)) (my-copy-region-or-follow-word)) ;;C-u
   (t (my-kill-region-or-follow-kill-word))))

(defun my-kill-region-or-forward-kill-word (arg)  
  "カーソル位置から前方を一単語削除"
  (interactive "p")  
  (if mark-active  
	  (let ((start (mark))  
			(end (point)))
		(kill-region start end))  
	(kill-word arg)))

(defun my-kill-region-or-backward-kill-word (arg)  
  "カーソル位置から後方を一単語削除"
  (interactive "p")
  (if mark-active  
	  (let ((start (mark)) (end (point)))
		(kill-region start end))
	(backward-kill-word arg)))

(defun my-delete-region-or-follow-kill-word ()
  "リージョンがアクティブでなければカーソルが乗っている単語を削除"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
	(save-excursion
	  (forward-word)
	  (let ((endpos (point)))
		(backward-word)
		(delete-region (point) endpos))
	  )))

(defun my-kill-region-or-follow-kill-word ()
  "リージョンがアクティブでなければカーソルが乗っている単語を切り取り"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (mark) (point))
	(save-excursion
	  (forward-word)
	  (let ((endpos (point)))
		(backward-word)
		(kill-region (point) endpos))
	  )))

(defun my-copy-region-or-follow-word ()
  "リージョンがアクティブでなければカーソルが乗っている単語をコピー"
  (interactive)
  (if (and transient-mark-mode mark-active)
      (copy-region-as-kill (mark) (point))
	(save-excursion
	  (forward-word)
	  (let ((endpos (point)))
		(backward-word)
		(copy-region-as-kill (point) endpos))
	  )))

(defun my-yank ()
  "リージョンがアクティブなら削除してヤンク"
  (interactive)
  (progn
	(if (and transient-mark-mode mark-active)
		(delete-region (mark) (point))
	  )
	(yank)
	))

(defun my-delete-region-or-delete-char ()
  ""
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
	(delete-char 1)))

(defun my-delete-region-or-backward-char ()
  ""
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
	(delete-backward-char 1)))

;;行の削除関数
(defun my-delete-line-backward-private ()
  "delete char backward line private."
  (interactive)
  (if
      (eq (bolp) nil)
      (progn
	(delete-char -1)
	     (my-delete-line-backward-private))))

(defun my-delete-line-backward ()
  "delete char backward line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark)(point))
    (if
	(eq (bolp) nil)
	(progn
	  (backward-char)
	  (delete-char 1)
	  (my-delete-line-backward-private))
      (delete-char -1))))


(defun my-delete-line-forward ()
  "delete char forward line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark)(point))
	(if (eq (line-end-position) (point))
		(delete-char 1)
		(delete-region (line-end-position)(point)))
	))

(defun my-delete-line ()
  "delete char line."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
    (my-delete-line-forward)
    (my-delete-line-backward-private)))

(defun my-delete-forward-word ()
  "delete char forward word."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
	(let ((start-pos (point)))
	  (forward-word)
      (delete-region start-pos (point)))))

(defun my-delete-backward-word ()
  "delete backward word."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (delete-region (mark) (point))
    (let ((start-pos (point)))
      (backward-word)
	  (delete-region (point) start-pos))))

;;;-------------------------------
;;; insert-space
;;;-------------------------------
(defun my-just-one-space-toggle ()
  "カーソル位置にスペース、ピリオド、ハイフンを順に挿入する関数"
  (interactive)
  (progn
;;	(message (concat "my-just-one-space-toggle"))
	(if (eq last-command 'my-just-one-space-toggle)
		(backward-char))
	(let ((char (following-char)))
	  (cond
	   ((= char ?\ )
		(progn (delete-char 1) (insert "_")))
	   ((= char ?_)
		(progn (delete-char 1) (insert "-")))
	   ((= char ?-)
		(progn (delete-char 1) (insert "　")))
	   ((= char ?\　)
		(progn (delete-char 1) (insert " ")))
	   (t (insert " "))))))

;;;-------------------------------------------------------------
;;; move-accelarator
;;; http://www.bookshelf.jp/soft/meadow_31.html#SEC420
;;;-------------------------------------------------------------
;; x 回ごとに加速
(defvar scroll-speedup-count 10)
;; x 回下カーソルを入力すると，次からは 1+1 で y 行ずつの
;; 移動になる
(defvar scroll-speedup-rate 1)
;; Zms 経過したら通常のスクロールに戻す
(defvar scroll-speedup-time 400)

;; 以下，内部変数
(defvar scroll-step-default 1)
(defvar scroll-step-count 1)
(defvar scroll-speedup-zero (current-time))

(defun scroll-speedup-setspeed ()
  (let* ((now (current-time))
         (min (- (car now)
                 (car scroll-speedup-zero)))
         (sec (- (car (cdr now))
                 (car (cdr scroll-speedup-zero))))
         (msec
          (/ (- (car (cdr (cdr now)))
                (car
                 (cdr (cdr scroll-speedup-zero))))
                     1000))
         (lag
          (+ (* 60000 min)
             (* 1000 sec) msec)))
    (if (> lag scroll-speedup-time)
        (progn
          (setq scroll-step-default 1)
          (setq scroll-step-count 1))
      (setq scroll-step-count
            (+ 1 scroll-step-count)))
    (setq scroll-speedup-zero (current-time))))

(defun scroll-speedup-next-line (arg)
  (if (= (% scroll-step-count
            scroll-speedup-count) 0)
      (setq scroll-step-default
            (+ scroll-speedup-rate
               scroll-step-default)))
  (if (string= arg 'next)
      (line-move scroll-step-default)
    (line-move (* -1 scroll-step-default))))

(defadvice next-line
  (around next-line-speedup activate)
  (if (and (string= last-command 'next-line)
           (interactive-p))
      (progn
        (scroll-speedup-setspeed)
        (condition-case err
            (scroll-speedup-next-line 'next)
          (error
           (if (and
                next-line-add-newlines
                (save-excursion
                  (end-of-line) (eobp)))
               (let ((abbrev-mode nil))
                 (end-of-line)
                 (insert "¥n"))
             (line-move 1)))))
    (setq scroll-step-default 1)
    (setq scroll-step-count 1)
    ad-do-it))

(defadvice previous-line
  (around previous-line-speedup activate)
  (if (and
       (string= last-command 'previous-line)
       (interactive-p))
      (progn
        (scroll-speedup-setspeed)
        (scroll-speedup-next-line 'previous))
    (setq scroll-step-default 1)
    (setq scroll-step-count 1)
    ad-do-it))

(defun my-emacs-eval-buffer ()
  ""
  (interactive)
  (find-file "~/.emacs")
  (eval-buffer)
  )

;; file-name-insert
(defun my-insert-this-file-name-nondirectory ()
   (interactive)
  ""
  (insert (file-name-nondirectory (buffer-file-name (current-buffer)))))

(defun my-insert-this-file-name-only-directory ()
   (interactive)
  ""
  (insert (file-name-directory (buffer-file-name (current-buffer)))))


(defun my-get-this-file-name-non-extension ()
   (interactive)
  ""
  (let ((name (file-name-nondirectory (buffer-file-name (current-buffer)))))
    (substring name 0 (- (length name) (length (file-name-extension name "."))))))

(defun my-insert-this-file-name ()
   (interactive)
  ""
  (insert (buffer-file-name (current-buffer))))

(defun my-get-include-guard-file-name ()
   (interactive)
  ""
  (replace-in-string (upcase (file-name-nondirectory (buffer-file-name (current-buffer))))
	   "\\." "_"))

(defun my-bookmark-set-with-default-name ()
  "bookmark-set with default name.
default name is filename:current line string."
  (interactive)
  (bookmark-set
   (concat 
	(file-name-nondirectory (buffer-file-name (current-buffer)))
	":"
	(car (split-string (thing-at-point 'line) "\n")))))

(defun my-split-window-for-buffer (buffer-name)
  "特定バッファのウィンドウがなければウィンドウ分割を行ってバッファを用意"
  (interactive)
  (if (eq (get-buffer-window buffer-name) nil)
      (split-window)
    )
  )

(defun my-shell-with-split-window ()
  "shell実行と共にウィンドウ分割"
  (interactive)
  (my-split-window-for-buffer "*shell*")
  (shell)
  )

(defun my-kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
	(save-restriction
	  (save-match-data
		(progn
		  (re-search-backward "[^ \t\r\n]" nil t)
		  (re-search-forward "[ \t\r\n]+" nil t)
		  (replace-match "" nil nil))))))

(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
	(if (not (bolp)) (forward-line 1))
	(delete-whitespace-rectangle (point) end nil)))

(defun my-backward-delete-char-hungry (arg &optional killp)
  "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
  (interactive "*p\nP")
  (let ((backward-delete-char-untabify-method 'hungry))
	(backward-delete-char-untabify arg killp)))

(defun my-unix-to-w32-pathname-transformer (args)
  ""
  (if (eq system-type 'windows-nt)
	  (concat "" (replace-regexp-in-string "/" "\\\\\\\\" args))
	args))

(defun my-open-directory-by-explorer ()
  ""
  (interactive)
  (shell-command
   (concat "explorer "
		   (my-unix-to-w32-pathname-transformer
			(expand-file-name (file-name-directory (buffer-file-name (current-buffer))))))))

(defun my-tsp-run-command-over-lines (command &optional n)
  "Run COMMAND over the following N lines.
    We use the previous N lines if N is negative."
  (interactive "CCommand: \np\n")
  (let ((a (if (< n 0)
			   (line-end-position)
			 (line-beginning-position)))
		(b (if (< n 0)
			   (line-beginning-position n)
			 (line-end-position n))))
	(save-restriction
	  (narrow-to-region a b)
	  (call-interactively command))))

(defun my-buffer-cygstart-exe ()
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((string= major-mode 'dired-mode)
      (if (string-match "^\\([a-z]:\\)/$" default-directory)
          (start-process "explorer" "diredcygstart" "explorer.exe"
                         (match-string 1  default-directory))
        (start-process "explorer" "diredcygstart" "explorer.exe"
                       (my-unix-to-w32-pathname-transformer
						(expand-file-name
						 (directory-file-name
						  default-directory))))))
     ((and file
           (file-exists-p file)
		   (start-process "cygstart" "diredcygstart" "cygstart.exe"
						  file)))
	  ((not file)
	   (error
		"現在のバッファはファイルではありません"))
	  ((file-directory-p file)
	   (start-process
		"explorer" "diredcygstart" "explorer.exe"
		(my-unix-to-w32-pathname-transformer (expand-file-name file))))
	  ((file-exists-p file)
	   (start-process
		"cygstart" "diredcygstart" "cygstart.exe" file))
	  ((not (file-exists-p file))
	   (error "ファイルが存在しません")))))

(defun my-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))

   (t (self-insert-command (or arg 1)))))

(defun my-get-buffer-point (buffer)
  (let ((cb (current-buffer)))
	(set-buffer buffer)
	(let ((ret-point (point)))
	  (set-buffer cb)
	  ret-point)))

(defun my-get-buffer-line (buffer)
  (let ((cb (current-buffer)))
	(set-buffer buffer)
	(let ((ret-line (current-line)))
	  (set-buffer cb)
	  ret-line)))

(defun my-max (a b)
  (if (> a b) a b))

(defun my-min (a b)
  (if (< a b) a b))

(defun my-insert-bracket-word ()
  ""
  (interactive)
  (progn
	(insert "[")
	(forward-word)
	(insert "]")))

(defun my-insert-bracket-char ()
  ""
  (interactive)
  (progn
	(insert "[")
	(forward-char)
	(insert "]")))


;;;-------------------------------
;;; eval
;;;-------------------------------
(defun my-eval-buffer-or-region ()
  "リージョンがアクティブならeval-region、アクティブでなければeval-buffer"
  (interactive)
  (if mark-active
	  (eval-region (mark) (point))
	(eval-current-buffer)))

(when (my-is-windows)
  (defun my-open-file-os (&optional filename)
    "osの関連付けを利用してファイルを開く"
    (interactive
     (save-excursion
       (let*
	   ((filename (if (eq filename nil)
			  (find-file-read-args "Find file: "
					       (confirm-nonexistent-file-or-buffer))
			filename
			))
	    (value (find-file-noselect (car filename) nil nil nil)))
	 (if (listp value)
	     (mapcar 'switch-to-buffer (nreverse value))
	   (switch-to-buffer value)
	   (my-buffer-cygstart-exe)
	   (kill-buffer (current-buffer)))))))
  )

(when (my-is-mac)
  (defun my-open-file-os (filename &optional isdir)
    "osの関連付けを利用してファイルを開く mac 用"
    (interactive)
    (let ((path (
		 (if isdir (directory-file-name filename) filename))))
    (shell-command (concat "open " filename)))))

(defun my-open-dir-os (path)
  "osの関連付けを利用してファイルを開く mac 用"
  (interactive)
    (shell-command (concat "open " (file-name-directory path))))

(defun my-other-window-or-split ()
  "別のウィンドウへ移動、ウィンドウがなければ分割"
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(provide 'init-my-misc)