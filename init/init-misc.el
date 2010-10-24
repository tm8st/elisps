;;; init-misc.el --- misc customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

(require 'doc-view)

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
;; (hiwin-mode) ; 起動時から有効にしたい場合

;; (require 'detect-block)
;; (detect-block t)

;; (auto-install-from-url "http://github.com/fukamachi/dont-type-twice-el/raw/master/dont-type-twice.el")
;; (require 'dont-type-twice)
;; (global-dont-type-twice t)

;; (auto-install-from-url "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")
;; (install-elisp "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")

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

(global-set-key (kbd "C-q C-t C-t") 'text-translator)
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
(global-set-key (kbd "[") (smartchr '("[`!!']" "[" "]")))
;; (global-set-key (kbd "]") (smartchr '("]" "[`!!']" "[]")))

;; (global-set-key (kbd "-") (smartchr '("-" "--" "---")))
;; (global-set-key (kbd "C-,") `my-replace-string)
(global-set-key (kbd "C-,") '(lambda () (interactive) (insert "_")))

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
(window-numbering-mode 1)
;; (global-set-key (kbd "C-l C-b C-0") 'select-window-0)
;; (global-set-key (kbd "C-l C-b C-1") 'select-window-1)
;; (global-set-key (kbd "C-l C-b C-2") 'select-window-2)
;; (global-set-key (kbd "C-l C-b C-3") 'select-window-3)
;; (global-set-key (kbd "C-l C-b C-4") 'select-window-4)
;; (global-set-key (kbd "C-l C-b C-5") 'select-window-5)
;; (global-set-key (kbd "C-l C-b C-6") 'select-window-6)
;; (global-set-key (kbd "C-l C-b C-7") 'select-window-7)
;; (global-set-key (kbd "C-l C-b C-8") 'select-window-8)
;; (global-set-key (kbd "C-l C-b C-9") 'select-window-9)

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
;;; 日本語入力
;;;-------------------------------
;; (require 'quail)
;; (define-key quail-translation-keymap (kbd "C-h") 'quail-conversion-backward-char)
;; (define-key quail-conversion-keymap (kbd "C-h") 'quail-conversion-backward-char)

;; (require 'sticky)
;; (use-sticky-key ";" sticky-alist:ja)
;; (use-sticky-key ";" sticky-alist:ja)

;; (require 'follow)
;; (global-set-key (kbd "C-l C-l") 'follow-delete-other-windows-and-split)
;; (global-set-key (kbd "C-l C-l") 'follow-delete-other-windows-and-split)

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

(let ((map graphviz-dot-mode-map))
	(define-key map (kbd "C-c C-p")    'graphviz-dot-preview)
	(define-key map (kbd "C-c C-c")    'compile)
	(define-key map (kbd "C-c C-e")    'my-graphbiz-execute)
	(define-key map (kbd "C-c C-v")    'graphviz-dot-view)
	(define-key map (kbd "C-m")  	   'my-backward-word)
	)

(defun my-graphbiz-execute ()
  (interactive)
	(compile (concat "dot -Tpng " buffer-file-name " > " (my-get-file-name-non-extension buffer-file-name) ".png"))
	;; (graphviz-dot-preview)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 二分移動
(defvar binary-move-fence-beginning nil)
(defvar binary-move-fence-end nil)
(defvar binary-move-overlay nil)
(defvar binary-move-face-color "gray20")

(defun binary-move-set-overlay ()
  (if binary-move-overlay
      (move-overlay binary-move-overlay 
                    binary-move-fence-beginning binary-move-fence-end
                    (current-buffer))
    (progn
      (setq binary-move-overlay
            (make-overlay binary-move-fence-beginning 
                          binary-move-fence-end))
      (make-face 'binary-move-face)
      (set-face-background 'binary-move-face binary-move-face-color)
    )
    (overlay-put binary-move-overlay 'face 'binary-move-face))
  )

(defun binary-move-forward (arg)
  (interactive "p")
  (while (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
    (next-line))
  (dotimes (i arg)
    (if (or
         (eq last-command 'binary-move-forward)
         (eq last-command 'binary-move-backward))
        (setq binary-move-fence-beginning (point))
      (progn
        (setq binary-move-fence-beginning (point))
        (setq binary-move-fence-end (line-end-position))))
    (binary-move-set-overlay)
    (goto-char (/ (+ binary-move-fence-beginning 
                     binary-move-fence-end) 2))
    (if (= (1- arg) i) (sit-for 1.0))
    (move-overlay binary-move-overlay (point) (point))
    ))

(defun binary-move-backward (arg)
  (interactive "p")
  (while (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
    (previous-line)
    (end-of-line))
  (dotimes (i arg)
    (if (or
         (eq last-command 'binary-move-forward)
         (eq last-command 'binary-move-backward))
        (setq binary-move-fence-end (point))
      (progn
        (setq binary-move-fence-beginning (line-beginning-position))
        (setq binary-move-fence-end (point))))
    (binary-move-set-overlay)
    (goto-char (/ (+ binary-move-fence-beginning 
                     binary-move-fence-end) 2))
    (if (= (1- arg) i) (sit-for 1.0))
    (move-overlay binary-move-overlay (point) (point))
    ))

(global-set-key (kbd "C-S->")  'binary-move-forward)
(global-set-key (kbd "C-S-<")  'binary-move-backward)

;; (require 'inertial-scroll)
;; (inertias-global-minor-mode 1)
;; (inertias-global-minor-mode 0)

(defun my-bs-cycle (arg)
  "バッファ進む、戻るの C-uによる選択用"
  (interactive "P")
  (cond
   ((equal arg '(4)) (bs-cycle-next))
   (t (bs-cycle-previous))))

;; (global-set-key (kbd "C-^") 'bs-cycle-previous)
;; (global-set-key (kbd "C-¥") 'bs-cycle-next)
(global-set-key (kbd "C--") 'my-bs-cycle)

;; (global-set-key [?\C-,] 'bs-cycle-next)
;; (global-set-key [?\C-.] 'bs-cycle-previous)
;; (global-set-key "\C-x\C-b" 'bs-show)

(require 'zlc)
(setq zlc-select-completion-immediately t)

(provide 'init-misc)
