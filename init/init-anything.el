;;; init-anything.el --- my anything setting.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, anything

;;; Commentary:

;;; Code:

;;;-------------------------------
;;; libs
;;;-------------------------------

(require 'anything)
(require 'anything-config)
(require 'anything-auto-install)
(require 'anything-grep)
(require 'anything-gtags)
(require 'anything-etags)
(require 'anything-extension)
(require 'anything-complete)
(require 'anything-kyr)
(require 'imenu)
(require 'imenu+)

;; (require 'anything-show-completion)
;; (require 'anything-dabbrev-expand)
;; (require 'anything-migemo)
;; (require 'anything-startup)
;; (require 'anything-kyr)
;; (require 'anything-kyr-config)
;; (require 'anything-adaptive)
;; (require 'anything-c-shell-history)
;; (require 'anything-include)

;;;-------------------------------
;;; basic
;;;-------------------------------
(setq anything-enable-shortcuts 'alphabet)
(setq anything-idle-delay 0.05)			;; 候補作成後の表示までの待機時間
(setq anything-input-idle-delay 0.05)		;; 検索結果表示更新までの待機時間
(setq anything-candidate-number-limit 1000)	;; 表示する候補数
;; (setq anything-samewindow nil) ;; 現在ウィンドウにanything window を作るか
;; (setq anything-samewindow t) ;; 現在ウィンドウにanything window を作るか
;; (setq anything-enable-digit-shortcuts nil) ;; 数字キーによるショートカットを使用するか
(setq anything-display-source-at-screen-top t) ;; 
(setq anything-persistent-action-use-special-display t)
(setq anything-execute-action-at-once-if-one t)

(setq anything-grep-multiline nil)
;; (setq anything-grep-fontify-file-name t)
(setq anything-scroll-amount 1) ;; スクロールは一行単位
(setq anything-quick-update t)

;;; 負荷軽減用
(defadvice anything-check-minibuffer-input
  (around sit-for activate)
  (if
      (sit-for anything-idle-delay t) ad-do-it))

;;;-------------------------------
;;; faces
;;;-------------------------------
(when use-gui-setting
  (set-face-foreground 'anything-file-name "Pink")
  (set-face-foreground 'anything-dir-heading "SkyBlue")
  (set-face-foreground 'anything-dir-priv "SkyBlue")
  )

;;;-------------------------------
;;; anything color moccur 
;;;-------------------------------

;;; color-moccur.elの設定
(require 'color-moccur)
;; 複数の検索語や、特定のフェイスのみマッチ等の機能を有効にする
;; 詳細は http://www.bookshelf.jp/soft/meadow_50.html#SEC751
(setq moccur-split-word t)
;; (setq moccur-split-word nil)

;; migemoがrequireできる環境ならmigemoを使う
;; (when (require 'migemo nil t) ;第三引数がnon-nilだとloadできなかった場合にエラーではなくnilを返す
;;  (setq moccur-use-migemo t))

;;; anything-c-moccurの設定
(require 'anything-c-moccur)
;; カスタマイズ可能変数の設定(M-x customize-group anything-c-moccur でも設定可能)
(setq anything-c-moccur-anything-idle-delay 0.2 ;`anything-idle-delay'
      anything-c-moccur-higligt-info-line-flag t ; `anything-c-moccur-dmoccur'などのコマンドでバッファの情報をハイライトする
      anything-c-moccur-enable-auto-look-flag t ; 現在選択中の候補の位置を他のwindowに表示する
      anything-c-moccur-enable-initial-pattern t) ; `anything-c-moccur-occur-by-moccur'の起動時にポイントの位置の単語を初期パターンにする

;;; キーバインドの割当(好みに合わせて設定してください)
;; (global-set-key (kbd "C-q C-a C-l") 'anything-occur) ;バッファ内検索
(global-set-key (kbd "C-q C-a C-o") 'anything-c-moccur-occur-by-moccur) ;バッファ内検索
(global-set-key (kbd "C-q C-a C-u") 'anything-c-moccur-dmoccur) ;ディレクトリ

(add-hook 'dired-mode-hook ;dired
          '(lambda ()
             (local-set-key (kbd "O") 'anything-c-moccur-dired-do-moccur-by-moccur)))

;;;-------------------------------
;;; gtags
;;;-------------------------------
(setq anything-gtags-enable-initial-pattern t)
(setq anything-gtags-classify nil) ;; fileでグループ分けを行う。
(setq gtags-path-style 'absolute)

;;;-------------------------------
;;; anything-favolite-directories
;;;-------------------------------
(defun anything-c-favolite-directories-display-to-real (line)
  (cdr (assq (intern (car (split-string line))) favolite-directory-assoc-template-list)))

(defun anything-dired-persistent-action (candidate)
  "Open subtree CANDIDATE without quitting anything. If CANDIDATE is a directory."
  (dired (file-name-directory candidate))
  (dired-goto-file candidate))

(defvar anything-c-source-favolite-directories
      '((name . "FavoliteDirectory")
	(candidates . (lambda () (mapcar '(lambda (i) (symbol-name (car i))) favolite-directory-assoc-template-list)))
	(display-to-real . anything-c-favolite-directories-display-to-real)
	(action . anything-c-point-file-in-dired)
	(persistent-action . anything-dired-persistent-action)
	(type . file))
      "短縮名をつけれるお気に入りディレクトリリスト"
      )

;; (anything anything-c-source-favolite-directories)

;;;-------------------------------
;;; anything map
;;;-------------------------------
(define-key anything-map (kbd "C-p") 'anything-previous-line)
(define-key anything-map (kbd "C-n") 'anything-next-line)
(define-key anything-map (kbd "C-;") 'anything-previous-page)
(define-key anything-map (kbd "C-v") 'anything-next-page)
(define-key anything-map (kbd "M-v") 'anything-next-source)
(define-key anything-map (kbd "M-;") 'anything-previous-source)
(define-key anything-map (kbd "<RET>") 'anything-exit-minibuffer)
(define-key anything-map (kbd "C-i") 'anything-select-action)
(define-key anything-map (kbd "C-h") 'delete-backward-char)
(define-key anything-map (kbd "C-o") 'anything-execute-persistent-action)
(define-key anything-map (kbd "C-c C-w") 'my-anything-delete-other-windows)
(define-key anything-c-moccur-anything-map (kbd "C-c C-w") 'my-anything-delete-other-windows)

(define-key anything-map (kbd "C-M-n") 'anything-scroll-other-window)
(define-key anything-map (kbd "C-M-p") 'anything-scroll-other-window-down)
(define-key anything-map (kbd "C-S-p") 'anything-previous-history-element)
(define-key anything-map (kbd "C-S-n") 'anything-next-history-element)
(define-key anything-map (kbd "C-@") 'anything-toggle-visible-mark)
(define-key anything-map (kbd "M-[") 'anything-prev-visible-mark)
(define-key anything-map (kbd "M-]") 'anything-next-visible-mark)

(define-key anything-map (kbd "C-s") 'anything-isearch)

(define-key anything-map (kbd "C-c C-d") 'anything-delete-current-selection)
(define-key anything-map (kbd "C-c C-y") 'anything-yank-selection)
(define-key anything-map (kbd "C-c C-k") 'anything-kill-selection-and-quit)

;;;-------------------------------
;;; keybinding
;;;-------------------------------
;; (global-set-key (kbd "C-q C-o") 'anything-dabbrev-expand)
;; (global-set-key (kbd "C-q C-a C-t") 'anything-top)
;; (global-set-key (kbd "C-q C-y")  'auto-install-from-emacswiki)
;; (global-set-key (kbd "C-q C-o")  'anything-dabbrev-expand)

;; 通常
(global-set-key (kbd "C-@") `anything)
(global-set-key (kbd "C-\\") `anything)

;; anything-auto-install
(global-set-key (kbd "C-q C-a C-y")  'anything-auto-install-from-emacswiki)
(global-set-key (kbd "C-q C-a C-S-y")  'anything-auto-install-from-library)

;; 標準ソース
(setq anything-sources
;; (anything-set-sources
 (list
  anything-c-source-favolite-directories       
  anything-c-source-recentf
  anything-c-source-buffers+
  anything-c-source-files-in-current-dir+
  ;; anything-c-source-file-name-history
  ;; anything-c-source-bookmarks
  ;; anything-c-source-extended-command-history
  ;; anything-c-source-ffap-guesser
  ;; anything-c-source-emacs-commands
  ;; anything-c-source-colors
  ))

;; buffer検索
(defun anything-for-buffers ()
  ""
  (interactive)
  (anything '(
	      anything-c-source-buffers+
	      )))

;;;-------------------------------
;;; anything-grep-find
;;;-------------------------------
;; grep-find用コマンドの設定/ windowsでfindコマンドを別名にしているため。
(defvar my-anything-grep-find-command nil)
(if (my-is-windows)
    (setq my-anything-grep-find-command "gnufind . -name \"*.*\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -a -not -name \"*.exe\" -exec grep -ni -e \"%s\" {} +")
  (setq my-anything-grep-find-command "find . -name \"*.*\" -a -type f -a -not -name \"*.svn*\" -a -not -name \"*.bin\" -a -not -name \"*.exe\" -exec grep -ni -e \"%s\" {} +")
  )

(defun my-anything-grep-find ()
  (interactive)
  (let ((command (format my-anything-grep-find-command (read-string "Search Text:")))
	(directory (read-directory-name "Directory: " default-directory default-directory t)))
    (message (concat "anything-grep-find:" command))
    (anything-grep command default-directory)))

;; ヘルプメニュー
(global-set-key (kbd "C-q C-a C-h") '(lambda () (interactive)
			       (let ((anything-sources (list anything-c-source-lacarte))) (anything))))

(global-set-key (kbd "C-q C-a C-g") 'anything-grep)
(global-set-key (kbd "C-q C-a C-f") 'my-anything-grep-find)
(global-set-key (kbd "C-q C-a C-a") 'anything-apropos)
(global-set-key (kbd "C-q C-a C-r") 'anything-resume) ;; ひとつ前のanythingに復帰
(global-set-key (kbd "C-q C-a C-j") 'anything-info-at-point)
(global-set-key (kbd "C-q C-a C-k") 'anything-kyr)
;; (global-set-key (kbd "C-q C-a C-n") 'anything-next-condidate) ;; ひとつ次の候補を実行

;; locate検索
;; (global-set-key (kbd "C-q C-a C-l") '(lambda () (interactive)
;; 				       (let ((anything-sources (list anything-c-source-locate))) (anything))))

;; buffer検索
;; (global-set-key (kbd "C-q C-a C-b") 'anything-for-bookmarks)
(global-set-key (kbd "C-q C-a C-b") 'anything-for-buffers)

(global-set-key (kbd "C-q C-a C-i") 'anything-for-imenu)
(global-set-key (kbd "C-q C-a C-c") 'anything-for-customize)

;; dabbrev
;; (global-set-key (kbd "C-o") 'anything-dabbrev-expand)
;; (global-set-key (kbd "C-q C-a C-o") 'anything-dabbrev-expand)
;; (global-set-key (kbd "C-q C-a C-i") 'anything-dabbrev-find-all-buffers)

(setq anything-dabbrev-idle-delay 0.0)
(setq anything-dabbrev-all-min-length 0)
(setq anything-dabbrev-expand-candidate-number-limit 100)

;;;-------------------------------
;;; project
;;;-------------------------------
(require 'anything-project)
(global-set-key (kbd "C-q C-a C-p") 'anything-project)

;; c++ 登録
(ap:add-project
 :name 'c++
 :look-for '("GTAGS" ".*\\.sln") ; or
 :include-regexp '("\\.h$" "\\.sln$" "\\.cpp$" "\\.c$" "\\.h$" "\\.inl$" "\\.fx$" "\\.ini$" "\\.txt$" "\\.uc$" "\\.usf$") ;or
 )

;; elisp 登録
(ap:add-project
 :name 'elisp
 :look-for '("\\.emacs") ; or
 :include-regexp '("\\.el$") ;or
 )
;; scala 登録
(ap:add-project
 :name 'scala
 :look-for '("\\.ensime") ; or
 :include-regexp '("\\.scala$") ;or
 )
;; elisp 登録
(ap:add-project
 :name 'elisp
 :look-for '("subdirs\\.el") ; or
 :include-regexp '("\\.el$") ;or
 )

;; ;; ruby 登録
;; (ap:add-project
;;  :name 'ruby
;;  :look-for '("GTAGS" ".rake") ; or
;;  :include-regexp '("\\.rb$") ;or
;;  )

;;;-------------------------------
;;; split-setting 
;;;-------------------------------
(require 'split-root)
(defvar anything-compilation-window-height-percent 50.0)

(defun anything-compilation-window-root (buf)
  (let ((anything-compilation-window
        (split-root-window (truncate (* (window-height)
                                        (/ anything-compilation-window-height-percent
                                           100.0))))))
    (set-window-buffer anything-compilation-window buf)))

(setq anything-display-function 'anything-compilation-window-root)

;;;-------------------------------
;;; howm
;;;-------------------------------
(require 'anything-howm)
(global-set-key (kbd "C-q C-a C-,") 'anything-howm-search)

;;;-------------------------------
;;; anything-window
;;;-------------------------------
(defun my-anything-window-operation-base (command)
  "Apply anything-buffer window  function base."
  (save-selected-window
    (select-window
	 (get-buffer-window (get-buffer anything-buffer)))
    (call-interactively command)))

(defun my-anything-delete-other-windows ()
  "Delete other window."
  (interactive)
  (my-anything-window-operation-base (lambda ()
			     (interactive)
			     (delete-other-windows))))

;; bookmarks+buffer検索
(defun anything-for-bookmarks ()
  ""
  (interactive)
  (anything '(
	      anything-c-source-bookmarks
	      anything-c-source-buffers+
	      )))

(defun anything-for-imenu ()
  ""
  (interactive)
  (anything '(
	      anything-c-source-imenu
	      )))

(setq imenu-max-items 100)
(setq imenu-max-item-length 160)

;;; customize
(defun anything-for-customize ()
  ""
  (interactive)
  (anything '(anything-c-source-customize-face
	      anything-c-source-colors
	      )))

;;; 現在のanythingの次の候補を実行
(defun anything-next-condidate ()
  ""
  (interactive)
  (anything-resume)
  (anything-next-line)
  (anything-select-action))

;; favolite dir
(defvar anything-c-source-favolite-directories
  '((name . "FavoliteDirectory")
    (candidates . `(lambda () (directory-files "~/")))
    (type . file)))

(defvar anything-c-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (apply 'start-process "locate-process" nil
                           (append anything-c-locate-options
                                   (list anything-pattern)))))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern with locate.")

(global-set-key (kbd "C-q C-a C-s") 'anything-call-source)

(require 'anything-netscape-bookmark)
(global-set-key (kbd "C-q C-a C-b") 'anything-netscape-bookmark)
(global-set-key (kbd "C-q C-a C-v") 'anything-netscape-bookmark-get-dump)

(provide 'init-anything)
