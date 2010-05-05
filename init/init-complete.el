;;; init-complete.el --- my complete setting.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, complete

;;; Commentary:

;;; Code:

(require 'complete) ;; 強力な補完機能を使う
(partial-completion-mode 1)
(icomplete-mode t) ;; 補完可能なものを随時表示

;;;-------------------------------------
;;;auto-complete setting
;;;-------------------------------------
(require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)
(global-auto-complete-mode t)
(customize-set-value 'ac-dictionary-directories (list "~/elisps/external/complete/m2ym-auto-complete-2c75fd1/dict"))

(setq ac-auto-start 3)
(customize-set-value 'ac-ignore-case 'smart)
(customize-set-value 'ac-candidate-limit 1000)
(customize-set-value 'ac-use-fuzzy 'nil)
(customize-set-value 'ac-use-comphist 'nil)
(customize-set-value 'ac-use-quick-help 'nil)
(customize-set-value 'ac-delay 0.2)

(global-set-key (kbd "C-o") 'ac-start)
;; (global-set-key (kbd "C-q C-c") 'auto-complete-mode)

(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-o") 'ac-previous)
(define-key ac-complete-mode-map (kbd "C-u") 'ac-stop)
(define-key ac-complete-mode-map (kbd "C-i") 'ac-next)
(define-key ac-complete-mode-map (kbd "C-j") 'ac-complete)

;; (require 'ac-anything)
;; (define-key ac-complete-mode-map (kbd "C-@") 'ac-complete-with-anything)

(set-face-foreground 'ac-candidate-face "White")
(set-face-foreground 'ac-selection-face "Pink")
(set-face-background 'ac-candidate-face "gray30")
(set-face-background 'ac-selection-face "gray30")

(require 'popup)
(set-face-foreground 'popup-menu-face "White")
(set-face-background 'popup-menu-face "gray30")
(set-face-background 'popup-menu-selection-face "gray30")
(set-face-foreground 'popup-menu-selection-face "Pink")
(set-face-background 'popup-menu-selection-face "gray30")

;;;-------------------------------------
;;;標準補間機能のカスタマイズ completion
;;;-------------------------------------
;;key binding
(define-key completion-list-mode-map "\C-n" 'next-completion)
(define-key completion-list-mode-map "\C-f" 'next-completion)
(define-key completion-list-mode-map "\C-p" 'previous-completion)
(define-key completion-list-mode-map "\C-b" 'previous-completion)
(define-key completion-list-mode-map "\C-m" 'my-choose-completion)

(defun my-choose-completion ()
  "Choose the completion that point is in or next to."
  (interactive)
  (let (beg end completion (buffer completion-reference-buffer)
	    (base-size completion-base-position))
    (if (and (not (eobp)) (get-text-property (point) 'mouse-face))
	(setq end (point) beg (1+ (point))))
    (if (and (not (bobp)) (get-text-property (1- (point)) 'mouse-face))
	(setq end (1- (point)) beg (point)))
	(if (null beg)
	    (error "No completion here"))
	(setq beg (previous-single-property-change beg 'mouse-face))
	(setq end (or (next-single-property-change end 'mouse-face) (point-max)))
	;; (setq pletion (buffer-substring-no-properties beg end))
	(delete-completion-window)
	(choose-completion-string completion buffer base-size)))

;; 動的補完で無視する要素の正規表現
(customize-set-value 'dabbrev-abbrev-skip-leading-regexp "-")

;;;-------------------------------
;;; company settings
;;;-------------------------------
;; (add-to-load-path-recompile "~/elisps/complete/company-0.4.3")
;; (add-to-load-path-recompile "~/elisps/complete/pysmell-0.7.3")

;; (defun my-company-select-previous-page ()
;;   ""
;;   (interactive)
;;   (let ((num company-how-many-completions-to-show))
;; 	(while (> num 1)
;; 	  (company-select-previous)
;; 	  (setq num (- num 1))
;; 	  )))

;; (defun my-company-select-next-page ()
;;   ""
;;   (interactive)
;;   (let ((num company-how-many-completions-to-show))
;; 	(while (> num 1)
;; 	  (company-select-next)
;; 	  (setq num (- num 1))
;; 	  )))

;; (require 'company)
;; ;; (require 'company-pysmell)

;; ;;(company-mode t)
;; (global-set-key (kbd "C-q C-c") 'company-mode) ;;toggle company mode
;; (global-set-key (kbd "C-l C-i") 'company-manual-begin)

;; ;; バックエンド設定
;; ;; (setq company-safe-backends
;; ;;   '((company-abbrev . "Abbrev")
;; ;;     (company-css . "CSS")
;; ;;     (company-dabbrev . "dabbrev for plain text")
;; ;;     (company-dabbrev-code . "dabbrev for code")
;; ;;     (company-eclim . "eclim (an Eclipse interace)")
;; ;;     (company-elisp . "Emacs Lisp")
;; ;;     (company-etags . "etags")
;; ;;     (company-files . "Files")
;; ;;     (company-gtags . "GNU Global")
;; ;;     (company-ispell . "ispell")
;; ;;     (company-keywords . "Programming language keywords")
;; ;;     (company-nxml . "nxml")
;; ;;     (company-oddmuse . "Oddmuse")
;; ;;     (company-pysmell . "PySmell")
;; ;;     (company-ropemacs . "ropemacs")
;; ;;     (company-semantic . "CEDET Semantic")
;; ;;     (company-tempo . "Tempo templates")
;; ;;     (company-xcode . "Xcode")))

;; ;; (setq company-backends
;; ;; 	  '(
;; ;; 		company-elisp
;; ;; 		;; company-nxml
;; ;; 		;; company-css
;; ;; 		;; company-eclim
;; ;; 		;; company-semantic
;; ;; 		;; company-xcode
;; ;; 		company-gtags
;; ;; 		company-etags
;; ;; 		company-dabbrev-code
;; ;; 		company-keywords
;; ;; 		company-files
;; ;; 		company-dabbrev
;; ;; 		company-dabbrev-other-buffers
;; ;; 		))

;; ;; 複数補完を出す前に待つ秒数？
;; (setq company-idle-delay .1)
;; ;; ;; 1個目を自動的に補完
;; (setq company-auto-expand t)
;; ;; 表示形式
;; (setq company-display-style 'pseudo-tooltip)
;; ;; 文字を打っているときに自動的に補完ウィンドウを出さない
;; (setq company-complete-on-edit nil)
;; ;; 補完をしたなら, すぐにツールチップを出す
;; (setq company-tooltip-delay 0)
;; ;; 一度に表示する補完の量(デフォルト10)
;; (setq company-how-many-completions-to-show 10)
;; ;; 
;; (setq company-show-numbers t)

;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-v") 'my-company-select-next-page)
;; (define-key company-active-map (kbd "C-;") 'my-company-select-previous-page)

;; ;; ;; キー割り当て
;; ;; (define-key company-mode-map "促t" 'ignore)
;; ;; (define-key company-mode-map [(hyper I)] 'company-expand-common)
;; ;; (define-key company-active-map "促C-n" 'company-cycle)
;; ;; (define-key company-active-map "促C-p" 'company-cycle-backwards)

;; ;; ;; 動作しやがらねえ
;; ;; (define-key company-active-map " "
;; ;;   '(lambda ()
;; ;;      (interactive)
;; ;;      (company-cycle company-how-many-completions-to-show)))
;; ;; (define-key company-active-map "x"
;; ;;   '(lambda()
;; ;;      (interactive)
;; ;;     (company-cycle-backwards company-how-many-completions-to-show)))

;; ;; (define-key company-active-map "促t" 'company-expand-top)
;; ;; (define-key company-active-map "促C-m" 'company-expand-top)
;; ;; (define-key company-active-map (kbd "<return>") 'company-expand-top)

;; ;; TODO(使うなら)
;; ;; どうも補完した後にcompany-modeで付けられたfaceが消えないようなので,
;; ;; ソースを調べて, defadviceで消すようにすること
;; ;; いや, エラーがでてるのが原因かも. もうちょっと待ってみよう. 更新されるまで.

;; ;; ;; color setting.
;; (set-face-foreground 'company-tooltip "gray50")
;; (set-face-background 'company-tooltip "gray20")
;; (set-face-foreground 'company-tooltip-selection "light green")
;; ;; (set-face-foreground 'company-tooltip-selection "gray80")
;; (set-face-background 'company-tooltip-selection "gray40")
;; (set-face-foreground 'company-tooltip-common "green")
;; (set-face-background 'company-tooltip-common "gray20")
;; (set-face-foreground 'company-tooltip-common-selection "green")
;; (set-face-background 'company-tooltip-common-selection "gray40")

;;-------------------------------
;;abbrev setting.
;;-------------------------------
;; (require 'abbrev)
;; ;;(require 'abbrev-complete)
;; ;;(require 'dabbrev-highlight)
;; ;;(load "dabbrev-ja")
;; (setq abbrev-file-name "~/.abbrev_defs")        ;;保存先を指定する
;; (quietly-read-abbrev-file)          ;;起動時に保存した略称を読み込む
;; (read-abbrev-file)              ;;略語ファイルを読み込み
;; (setq save-abbrevs t)               ;;略称を保存する
;; (abbrev-mode nil)                 ;;モードは実行しない

;;動的補間候補用ファイルの読み込み処理
;;(setq my-filename-for-abbrev
;;      '("~/GTAGS" "~/dabbrev-list.txt" "~/cscope.out")) ;ファイルリスト
;; (setq my-filename-for-abbrev nil)

;;(defun find-file-for-abbrev ()
;; (progn
;;   (let ((list my-filename-for-abbrev) filename buf cbuf)
;; 	(setq cbuf (current-buffer))
;; 	(while list
;; 	  (setq filename (car list))
;; 	  (setq list (cdr list))
;; 	  (if (get-buffer filename)
;; 		  ()
;; 		(progn
;; 		  (setq buf (find-file-noselect filename))
;; 		  (set-buffer buf)
;; 		  (rename-buffer
;; 		   (concat " *" (file-name-nondirectory filename) "*") t))))
;; 	(set-buffer cbuf)))
;; ;;; 履歴補完を使う
;; (require 'pabbrev)
;; (global-pabbrev-mode)

(provide 'init-complete)
