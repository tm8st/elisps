;;; init-misc.el --- misc customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

;最小の ewm 設定例
;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el")
;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-manager/raw/master/ewm.el")
;; (require 'ewm)
;; (global-set-key (kbd "M-+") 'ewm:start-management)

;; (require 'detect-block)
;; (detect-block t)

;; (auto-install-from-url "http://github.com/fukamachi/dont-type-twice-el/raw/master/dont-type-twice.el")
;; (require 'dont-type-twice)
;; (global-dont-type-twice t)

;; (auto-install-from-url "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")
;; (install-elisp "http://github.com/tomoya/hiwin-mode/raw/master/hiwin.el")

;; (require 'hiwin)
;; (setq hiwin-color "gray13")
;; (setq hiwin-color "darkslategreen")
;; (hiwin-mode nil) ; 起動時から有効にしたい場合
;; (hiwin-mode) ; 起動時から有効にしたい場合

(require 'text-translator)

(global-set-key (kbd "C-q C-t C-t") 'text-translator-translate-by-auto-selection)
(global-set-key (kbd "C-q C-t C-o") 'text-translator-all)
;; (global-set-key (kbd "C-q C-t C-e") 'text-translator-all)
;; (global-set-key (kbd "C-q C-t C-o") 'text-translator-all-by-auto-selection)

;; (global-set-key (kbd "C-q C-t C-o") '(lambda () (text-translator-all ))
;; (global-set-key (kbd "C-q C-t C-") 'text-translator-all)

;; プリフィックスキーを変更する場合.
;; (setq text-translator-prefix-key "\M-n")

;;;-------------------------------
;;; fastnav
;;;-------------------------------
;; (require 'fastnav)
;; (global-set-key (kbd "C-S-f") 'jump-to-char-forward)
;; (global-set-key (kbd "C-S-b") 'jump-to-char-backward)
;; (global-set-key "\M-z" 'zap-up-to-char-forward)
;; (global-set-key "\M-Z" 'zap-up-to-char-backward)
;; (global-set-key "\M-s" 'jump-to-char-forward)
;; (global-set-key "\M-S" 'jump-to-char-backward)
;; (global-set-key "\M-r" 'replace-char-forward)
;; (global-set-key "\M-R" 'replace-char-backward)
;; (global-set-key "\M-i" 'insert-at-char-forward)
;; (global-set-key "\M-I" 'insert-at-char-backward)
;; (global-set-key "\M-j" 'execute-at-char-forward)
;; (global-set-key "\M-J" 'execute-at-char-backward)
;; (global-set-key "\M-k" 'delete-char-forward)
;; (global-set-key "\M-K" 'delete-char-backward)
;; (global-set-key "\M-m" 'mark-to-char-forward)
;; (global-set-key "\M-M" 'mark-to-char-backward)

;;-------------------------------
;; smartchr
;;-------------------------------
(require 'smartchr)

;; substitute `!!' with cursor
(global-set-key (kbd "{") (smartchr '("{`!!'}" "{")))
(global-set-key (kbd "}") (smartchr '("}" "{`!!'}" "}")))
(global-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
(global-set-key (kbd "\'") (smartchr '("\'" "\'`!!'\'")))
(global-set-key (kbd "`") (smartchr `("`" "``!!'`")))
(global-set-key (kbd "(") (smartchr '("(`!!')" "(")))
(global-set-key (kbd ")") (smartchr '(")" "(`!!')" )))
(global-set-key (kbd "+") (smartchr '("+" "++" "+++")))
(global-set-key (kbd "[") (smartchr '("[`!!']" "[" "]")))
(global-set-key (kbd "]") (smartchr '("]" "[`!!']" "[]")))

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
(global-set-key (kbd "C-l C-j C-s") 'mark-string*)
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

(require 'follow)
(global-set-key (kbd "C-l C-l") 'follow-delete-other-windows-and-split)
;; (global-set-key (kbd "C-l C-l") 'follow-delete-other-windows-and-split)

;;;-------------------------------
;; keyboard-macro
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

(provide 'init-misc)
