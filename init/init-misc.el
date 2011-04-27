;;; init-misc.el --- misc customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

(require 'doc-view)

;;;-------------------------------
;;; popwin
;;;-------------------------------
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
;; (setq special-display-function 'popwin:special-display-popup-window)
(add-to-list 'popwin:special-display-config '("*Compile-Log*"))
(add-to-list 'popwin:special-display-config '("*Dired log*"))
(add-to-list 'popwin:special-display-config '("*Rake*"))

(define-key global-map (kbd "C-l p") 'popwin:display-last-buffer)

;;;-------------------------------
;;; text-translator
;;;-------------------------------
(require 'text-translator-vars)
(require 'text-translator)

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)

(require 'popup)
(setq text-translator-display-popup t)

(defun my-text-translator-toggle-popup ()
	"toggle display setting."
  (interactive)
  (setq text-translator-display-popup (not text-translator-display-popup))
  (message (concat "Text-Translator current popup " (if text-translator-display-popup "Enable" "Disable") ".")))

(defun my-text-translator-insert ()
	"insert last translated text."
	(interactive)
	(when text-translator-last-translated-text
	  (insert text-translator-last-translated-text)))

(defun my-text-translator-jaen ()
	"insert last translated text."
	(interactive)
	(text-translator nil nil "google.com_jaen"))

(defun my-text-translator-enja ()
	"insert last translated text."
	(interactive)
	(text-translator nil nil "google.com_enja"))

(global-set-key (kbd "C-q C-t C-t") 'my-text-translator-enja)
(global-set-key (kbd "C-q C-t C-i") 'my-text-translator-jaen)
(global-set-key (kbd "C-q C-t C-o") 'text-translator-all)
(global-set-key (kbd "C-q C-t C-u") 'my-text-translator-toggle-popup)
(global-set-key (kbd "C-q C-t C-y") 'my-text-translator-insert)

;;;-------------------------------
;;; yafastnav, jaunte
;;;-------------------------------
(require 'yafastnav)
(require 'jaunte)

(global-set-key (kbd "C-l C-h") 'jaunte)
(global-set-key (kbd "C-l C-r") 'yafastnav-jump-to-backward)
(global-set-key (kbd "C-l C-SPC") 'yafastnav-jump-to-forward)

;;-------------------------------
;; smartchr
;;-------------------------------
(require 'smartchr)

;; substitute `!!' with cursor
(global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))
;; (global-set-key (kbd "}") (smartchr '("}" "{`!!'}" "}")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "\'") (smartchr '("\'" "\'`!!'\'")))
(global-set-key (kbd "`") (smartchr `("`" "``!!'`")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
;; (global-set-key (kbd ")") (smartchr '(")" "(`!!')" )))
(global-set-key (kbd "+") (smartchr '("+" "++" "+++")))
;; (global-set-key (kbd "-") (smartchr '("-" "->" "<->")))
;; (global-set-key (kbd "-") (smartchr '("-" "--" "---")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[" "]")))
;; (global-set-key (kbd "]") (smartchr '("]" "[`!!']" "[]")))

;;;-------------------------------
;;; sequential-command-config
;;;-------------------------------
(require 'sequential-command)
(define-sequential-command beginning-of-anything-seq
  back-to-indentation beginning-of-line seq-return)

(define-sequential-command end-of-anything-seq
  end-of-line seq-return)

(require 'sequential-command-config)
(global-set-key (kbd "C-a") 'beginning-of-anything-seq)
(global-set-key (kbd "C-e") 'end-of-anything-seq)

;;;-------------------------------
;;; emacsから一発で検索
;;;-------------------------------
(require 'search-web)

(defun my-search-web (engine)
  (interactive)
  (let ((word (read-string "search-word:")))
    (browse-url
     (format
      (cdr (assoc engine search-engines)) (url-hexify-string word)))))

;; google
(define-key global-map (kbd "C-l C-s C-s") (lambda () (interactive) (my-search-web "g")))
;; 英辞郎
(define-key global-map (kbd "C-l C-s C-e") (lambda () (interactive) (my-search-web "eow")))
;; AMAZON
(define-key global-map (kbd "C-l C-s C-a") (lambda () (interactive) (my-search-web "zj")))
;; udn
(define-key global-map (kbd "C-l C-s C-u") (lambda () (interactive) (my-search-web "udn")))

;;;-------------------------------
;;; Profile
;;;-------------------------------
(require `elp)

(global-set-key (kbd "C-l C-a C-s") `elp-instrument-package)
(global-set-key (kbd "C-l C-a C-r") `elp-results)
(global-set-key (kbd "C-l C-a C-e") `elp-reset-all)

;;;-------------------------------
;;; windowナンバリング
;;;-------------------------------
(require 'window-numbering)
(window-numbering-mode t) ;; M-numberで移動

;;;-------------------------------
;;; region selectinon
;;;-------------------------------
(require 'thing-opt)
(define-thing-commands)
(global-unset-key (kbd "C-l C-j"))
(global-set-key (kbd "C-l C-j C-w") 'mark-word*)
(global-set-key (kbd "C-l C-j C-e") 'mark-sexp*)
(global-set-key (kbd "C-l C-j C-s") 'mark-string)
(global-set-key (kbd "C-l C-j C-f") 'mark-defun*)

;;;-------------------------------
;; keyboard-macro
;;;-------------------------------
;; C-x (
;; キーボードマクロの定義を開始する （start-kbd-macro）。
;; C-x )
;; キーボードマクロの定義を終了する （end-kbd-macro）。
;; C-x e
;; もっとも最近のキーボードマクロを実行する （call-last-kbd-macro）。
;; C-u C-x (
;; もっとも最近のキーボードマクロを再実行したうえで、 その定義にキーを追加する。
;; C-x q
;; キーボードマクロの実行中にこの場所に到達したら、 実行の確認を求める （kbd-macro-query）。
;; M-x name-last-kbd-macro
;; もっとも最近に定義したキーボードマクロに（現在のEmacsセッションだけで有効な） コマンド名を与える。
;; M-x insert-kbd-macro
;; キーボードマクロの定義をLispコードとしてバッファに挿入する。
;; C-x C-k
;; まえに定義したキーボードマクロを編集する （edit-kbd-macro）。
;; M-x apply-macro-to-region-lines
;; リージョン内の各行に対して、最後に定義したキーボードマクロを実行する。
;;;-------------------------------
(defun my-last-kbd-macro-name-and-insert ()
  (interactive)
  (let ((name (read-string "Macro Name is:")))
    (name-last-kbd-macro name)
    (insert-kbd-macro name)
    ))

(global-set-key (kbd "C-q C-8") 'start-kbd-macro)
(global-set-key (kbd "C-q C-9") 'end-kbd-macro)
(global-set-key (kbd "C-q C-0") 'my-last-kbd-macro-name-and-insert)

;;;-------------------------------
;;; graphviz mode
;;;-------------------------------
(require 'graphviz-dot-mode)

(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(customize-set-variable 'graphviz-dot-indent-width 2)
(add-hook 'graphviz-dot-mode-hook (setq tab-width 2))

(defun my-graphbiz-execute ()
  (interactive)
	(compile (concat "dot -Tpng " buffer-file-name " > " (my-get-file-name-non-extension buffer-file-name) ".png"))
	;; (graphviz-dot-preview)
  )

(let ((map graphviz-dot-mode-map))
	(define-key map (kbd "C-c C-p")    'graphviz-dot-preview)
	(define-key map (kbd "C-c C-c")    'compile)
	(define-key map (kbd "C-c C-e")    'my-graphbiz-execute)
	(define-key map (kbd "C-c C-v")    'graphviz-dot-view)
	(define-key map (kbd "C-m")  	     'my-backward-word)
;; (define-key map "\r"       'electric-graphviz-dot-terminate-line)
;; (define-key map "{"        'electric-graphviz-dot-open-brace)
;; (define-key map "}"        'electric-graphviz-dot-close-brace)
;; (define-key map ";"        'electric-graphviz-dot-semi)
;; (define-key map "\M-\t"    'graphviz-dot-complete-word)
;; (define-key map "\C-\M-q"  'graphviz-dot-indent-graph)
;; (define-key map "\C-cp"    'graphviz-dot-preview)
;; (define-key map "\C-cc"    'compile)
;; (define-key map "\C-cv"    'graphviz-dot-view)
;; (define-key map "\C-c\C-c" 'comment-region)
;; (define-key map "\C-c\C-u" 'graphviz-dot-uncomment-region)
	)

(defun my-bs-cycle (arg)
  "バッファ進む、戻るの C-uによる選択用"
  (interactive "P")
  (cond
   ((equal arg '(4)) (bs-cycle-next))
   (t (bs-cycle-previous))))

;; (global-set-key [?\C-,] 'bs-cycle-next)
;; (global-set-key [?\C-.] 'bs-cycle-previous)
;; (global-set-key "\C-q\C-b" 'bs-show)
(global-set-key (kbd "C--") 'my-bs-cycle)

(require 'zlc)
(setq zlc-select-completion-immediately t)

;; auto-highlight-symbol
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(set-face-background ahs-face "gray30")
(set-face-foreground ahs-face "pink")
(customize-set-value 'ahs-idle-interval 0.5)
(add-to-list 'ahs-modes 'haskell-mode)

(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*Backtrace*" :height 0.5))

;;;-------------------------------
;;; frame-arrange
;;;-------------------------------
(require 'frame-arrange)
(when (my-is-mac)
	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client
	 '((top + -0) (left + -1700) (height . 720) (width . 113)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-full-screen
	 '((top + -0) (left + -1920) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-default
	 '((top + 0) (left + 0) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client-main
	 '((top + 0) (left + 0) (height . 1080) (width . 121)))
	)

(when (my-is-windows)
	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client
	 '((top + -0) (left + 0) (height . 720) (width . 110)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-full-screen
	 '((top + -0) (left + -1680) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-default
	 '((top + 0) (left + 0) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client-main
	 '((top + 0) (left + 0) (height . 1080) (width . 121))))

(define-key global-map (kbd "C-l C-o C-m")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-with-twitter-client)))

(define-key global-map (kbd "C-l C-o C-f")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-full-screen)))

(define-key global-map (kbd "C-l C-o C-d")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-default)))

(define-key global-map (kbd "C-l C-o C-s")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-with-twitter-client-main)))


;;;-------------------------------
;;; Rakefile
;;;-------------------------------
(add-to-list 'auto-mode-alist '("\\Rakefile$\\'" . ruby-mode))
(define-key global-map (kbd "C-l C-o C-c")
  #'(lambda ()
      (interactive)
			(async-shell-command "rake -t" (get-buffer-create "*Rake*"))))

(define-key global-map (kbd "C-l C-o C-t")
  #'(lambda ()
      (interactive)
			(async-shell-command "rake -t test" (get-buffer-create "*Rake*"))))

(add-to-list 'auto-mode-alist '("RAKEFILE$" . ruby-mode))

;;;-------------------------------
;;; yalinum
;;;-------------------------------
(require 'yalinum)
(global-yalinum-mode t)
(global-linum-mode -1)

  ;; (start-process-shell-command
  ;;  "gtags-update"
  ;;  "*my-gtags-update*"
  ;;  (concat "cd " (gtags-get-rootpath) " && gtags -v"))

;;;-------------------------------
;;; vc keybind
;;;-------------------------------
;; C-x v v vc-next-action          次の動作 (commit)
;; C-x v d vc-directory            登録されたファイルを表示
;; C-x v = vc-diff                 diff表示
;; C-x v u vc-revert-buffer        checkinしたものに戻す
;; C-x v ~ vc-version-other-window 所定のrevを別のwindowへ
;; C-x v l vc-print-log            log表示
;; C-x v i vc-register             add
;; C-x v h vc-insert-headers       version headerを挿入
;; C-x v r vc-retrieve-snapshot    tag指定checkout
;; C-x v s vc-create-snapshot      tagをつける
;; C-x v c vc-cancel-version       保存されたrevを捨てる。
;; C-x v a vc-update-change-log    GNUスタイルでchangeLogを更新

;;;-------------------------------
;;; hiwin
;;;-------------------------------
;; (require 'hiwin)
;; (setq hiwin-color "gray30")
;; (setq hiwin-color "darkslategreen")
;; (hiwin-mode t) ; 起動時から有効にしたい場合

;; (require 'detect-block)
;; (detect-block t)

;; (auto-install-from-url "http://github.com/fukamachi/dont-type-twice-el/raw/master/dont-type-twice.el")
;; (require 'dont-type-twice)
;; (global-dont-type-twice t)

;; (auto-install-from-url "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")
;; (install-elisp "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")

(provide 'init-misc)
