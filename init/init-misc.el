;;; init-misc.el --- misc customize.

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

(my-require 'doc-view)
(my-require 'init-keybindings)

;;; text-mode
(add-hook
 'text-mode-hook
 (lambda ()
   (setq tab-width 4)
   (customize-set-value 'standard-indent 4)
   (setq indent-tabs-mode t)))

;;; popwin
(my-require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(add-to-list 'popwin:special-display-config '("*Compile-Log*"))
(add-to-list 'popwin:special-display-config '("*Dired log*"))
(add-to-list 'popwin:special-display-config '("*Rake*"))
(add-to-list 'popwin:special-display-config '("*Backtrace*" :height 0.5))
(add-to-list 'popwin:special-display-config '("*git push*"))
(add-to-list 'popwin:special-display-config '("*git commit*"))

(define-key global-map (kbd "C-l p") 'popwin:display-last-buffer)

;;; text-translator
(my-require 'text-translator-vars)
(my-require 'text-translator)
(my-require 'init-my-misc)

(my-require 'popup)
(setq text-translator-display-popup t)

;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)

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
	"taranslate ja -> en."
	(interactive)
	(text-translator nil nil "google.com_jaen"))

(defun my-text-translator-enja ()
	"taranslate en -> ja."
	(interactive)
	(text-translator nil nil "google.com_enja"))

(global-set-key (kbd "C-q C-t C-t") 'my-text-translator-enja)
(global-set-key (kbd "C-q C-t C-i") 'my-text-translator-jaen)
(global-set-key (kbd "C-q C-t C-o") 'text-translator-all)
(global-set-key (kbd "C-q C-t C-u") 'my-text-translator-toggle-popup)
(global-set-key (kbd "C-q C-t C-y") 'my-text-translator-insert)

(defun my-alc (word)
  (interactive "sTranslate: ")
  (let* ((url (concat "http://eow.alc.co.jp/" (replace-regexp-in-string " " "+" word) "/UTF-8/"))
	 (str (shell-command-to-string (concat "wget -q " url " -O -"))))
    (string-match "検索結果本体 ▼\\(\\w\\|\\W\\)*▲ 検索結果本体 ▲" str)
    (setq str (substring str (+ 22 (match-beginning 0)) (- (match-end 0) 23)))
    (setq str (replace-regexp-in-string "<\\(span\\|font\\)\\(\\w\\|\\W\\)*?>" "" str))
    (setq str (replace-regexp-in-string "</\\(span\\|font\\)>" "" str))
    (setq str (replace-regexp-in-string "</div></li>\\|<ol>\\|</ol>\\|</li><li>" "\n" str))
    (setq str (replace-regexp-in-string "<\\(\\w\\|\\W\\)*?>" "" str))
    (setq str (replace-regexp-in-string "^M" "\n" str))
    (setq str (my-delete-blank-lines str))
    (prog1
	(save-current-buffer
	  (save-selected-window
	    (with-current-buffer (get-buffer-create "*alc*"))
	    (pop-to-buffer (get-buffer "*alc*"))
	    (erase-buffer)
	    (insert str)
	    (goto-char (point-min))
	    )))
    ))

;; 消えちゃう。のでpopup-winにしない。
;; (add-to-list 'popwin:special-display-config '("*alc*"))
(global-set-key (kbd "C-q C-t C-@") 'my-alc)

(defun my-delete-blank-lines (str)
  (with-temp-buffer 
    (insert str)
    (goto-char (point-min))
    (re-search-forward "\\(^\\W*$\\|\\n\\)*")
    (delete-region (point-min) (match-end 0))
    (goto-char (point-max))
    (re-search-backward "\\w")
    (delete-region (+ 1 (point)) (point-max))
    (buffer-string)))

;;; yafastnav, jaunte
(my-require 'yafastnav)
(my-require 'jaunte)

(global-set-key (kbd "C-l C-h") 'jaunte)
(global-set-key (kbd "C-l C-r") 'yafastnav-jump-to-backward)
(global-set-key (kbd "C-l C-SPC") 'yafastnav-jump-to-forward)

;;-------------------------------
;; smartchr
;;-------------------------------
(my-require 'smartchr)
;; substitute `!!' with cursor

(global-set-key (kbd "{") (smartchr '("{`!!'}" "{`!!'")))
;; (global-set-key (kbd "}") (smartchr '("}`!!'" "{`!!'}" "`!!'}")))
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

;;; emacsから一発で検索
(my-require 'search-web)

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
;; UDN
(define-key global-map (kbd "C-l C-s C-u") (lambda () (interactive) (my-search-web "udn")))

;;; Windowナンバリング
(my-require 'window-numbering)
(window-numbering-mode t) ;; M-numberで移動

;;; region selectinon
(my-require 'thing-opt)
(define-thing-commands)
(global-set-key (kbd "C-l C-j C-w") 'mark-word*)
(global-set-key (kbd "C-l C-j C-e") 'mark-sexp*)
(global-set-key (kbd "C-l C-j C-s") 'mark-string)
(global-set-key (kbd "C-l C-j C-f") 'mark-defun*)

;; keyboard-macro
(defun my-last-kbd-macro-name-and-insert ()
  (interactive)
  (let ((name (read-string "Macro Name is:")))
    (name-last-kbd-macro name)
    (insert-kbd-macro name)
    ))

(global-set-key (kbd "C-q C-8") 'start-kbd-macro)
(global-set-key (kbd "C-q C-9") 'end-kbd-macro)
(global-set-key (kbd "C-q C-0") 'my-last-kbd-macro-name-and-insert)

;;; graphviz mode
(my-require 'graphviz-dot-mode)
(add-to-list 'auto-mode-alist '("\\.dot$" . graphviz-dot-mode))
(customize-set-variable 'graphviz-dot-indent-width 2)
(add-hook 'graphviz-dot-mode-hook (setq tab-width 2))

(defun my-graphbiz-execute ()
  (interactive)
	(compile (concat "dot -Tpic " buffer-file-name " > " (my-get-file-name-non-extension buffer-file-name) ".png"))
	(graphviz-dot-preview)
  )

(let ((map graphviz-dot-mode-map))
	(define-key map (kbd "C-c C-p")    'graphviz-dot-preview)
	(define-key map (kbd "C-c C-c")    'compile)
	(define-key map (kbd "C-c C-e")    'my-graphbiz-execute)
	(define-key map (kbd "C-c C-v")    'graphviz-dot-view)
	(define-key map (kbd "C-m")  	      my-backward-word-command)
  (define-key map (kbd "C-c C-r") '(lambda () (interactive) (insert-string " -> ")))
  (define-key map (kbd "C-c C-l") '(lambda () (interactive) (insert-string " <- ")))
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

(my-require 'zlc)
(setq zlc-select-completion-immediately t)

;; auto-highlight-symbol
(my-require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)
(set-face-background ahs-face "gray30")
(set-face-foreground ahs-face "pink")
(customize-set-value 'ahs-idle-interval 0.5)
(add-to-list 'ahs-modes 'haskell-mode)

;;; frame-arrange
(my-require 'frame-arrange)
(when (my-is-mac)
	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client
	 '((top + -0) (left + -1700) (height . 720) (width . 100)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-full-screen
	 '((top + -0) (left + -1920) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-default
	 '((top + 0) (left + 0) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client-main
	 '((top + 0) (left + 0) (height . 1080) (width . 97))))

(when (my-is-windows)
	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client
	 '((top + -0) (left + 0) (height . 720) (width . 100)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-full-screen
	 '((top + -0) (left + -1680) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-default
	 '((top + 0) (left + 0) (height . 1080) (width . 1920)))

	(frange:regist-frame-position-parameter
	 'my-frame-arrange-with-twitter-client-main
	 '((top + 0) (left + 0) (height . 1080) (width . 100))))

(define-key global-map (kbd "C-l C-w C-m")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-with-twitter-client)))

(define-key global-map (kbd "C-l C-w C-f")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-full-screen)))

(define-key global-map (kbd "C-l C-w C-d")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-default)))

(define-key global-map (kbd "C-l C-w C-s")
  #'(lambda ()
      (interactive)
      (frange:restore-frame-position-parameter
       (selected-frame)
       'my-frame-arrange-with-twitter-client-main)))

;;; Rakefile for Haskell.
(add-to-list 'auto-mode-alist '("\\RAKEFILE$\\'" . ruby-mode))
(defvar rake-task-alist '(("run") ("clean") ("clobber") ("test") ("profile") ("hpc")))
(defun my-run-rakefile ()
  (interactive)
  (async-shell-command
   (concat "rake -t "
           (completing-read "task?: " rake-task-alist nil t))
   (get-buffer-create "*Rake*")))

(define-key global-map (kbd "C-l C-o C-b") 'my-run-rakefile)

;;; yalinum
(my-require 'yalinum)
;; disable linum mode.
(global-linum-mode -1)
;; (global-yalinum-mode t)

;; buffer cycle move.
(my-require 'prefix-arg-commands)
(prefix-arg-commands-defun
 prefix-arg-commands-bs-cycle
 (list
  'bs-cycle-next
  'bs-cycle-previous
  ))

(prefix-arg-commands-defun
 prefix-arg-commands-buffer-cycle
 (list
  'next-buffer
  'previous-buffer
  ))

(global-set-key (kbd "C-^") 'prefix-arg-commands-buffer-cycle)
(global-set-key (kbd "C--") 'my-other-window-or-split)
;; (prefix-arg-commands-defun prefix-arg-commands-tabber-cycle
;;                             (list
;; 																'tabbar-forward
;; 																'tabbar-backward
;;                               ))
;; (global-set-key (kbd "C--") 'prefix-arg-commands-bs-cycle)
;; (global-set-key [C-tab] 'other-window) ;; window 切り替え
;; (global-set-key (kbd "C-x o") 'other-window)
;; (global-set-key (kbd "C-x p") '(lambda (arg) (interactive "p") (other-window (- arg))))
;; (global-set-key (kbd "C--") 'prefix-arg-commands-tabber-cycle)

;; (my-require 'type-se)

(defun my-output-last-command ()
  (interactive)
  (message (concat "ThisCommand    : " (symbol-name this-command)
                   "\nLastCommand    : " (symbol-name last-command)
                   "\nRealLastCommand: " (symbol-name real-last-command))))

(global-set-key (kbd "C-q C-3") 'my-output-last-command)

;; widen-window
;; (my-require 'widen-window)
;; (global-widen-window-mode t)
;; (my-require 'widen-window)
;; (global-set-key (kbd "C-c C-w")
;;                 `(lambda ()
;;                    (interactive) (global-widen-window-mode nil)))

;; (defun browse-url-at-point ()
;;   "カーソルのある位置のURLをブラウザで開く"
;;   (interactive)
;;   (let ((url-region (bounds-of-thing-at-point 'url)))
;;     (when url-region
;;       (browse-url (buffer-substring-no-properties (car url-region)
;; 						  (cdr url-region))))))

(global-set-key (kbd "C-c C-f") 'browse-url-at-point)

;; frame operations.
(global-set-key (kbd "C-l C-f C-n") 'make-frame-command)
(global-set-key (kbd "C-l C-f C-d") 'delete-frame)
(global-set-key (kbd "C-l C-f C-o") 'other-frame)

;;; hs-minor-mode
(my-require 'hideshow)
(defun my-hs-minor-mode-on () (hs-minor-mode 1))
(add-hook 'c-mode-hook 'my-hs-minor-mode-on)
(add-hook 'c++-mode-hook 'my-hs-minor-mode-on)
(add-hook 'haskell-mode-hook 'my-hs-minor-mode-on)
(add-hook 'emacs-lisp-mode-hook 'my-hs-minor-mode-on)
(add-hook 'ruby-mode-hook 'my-hs-minor-mode-on)

(define-key hs-minor-mode-map (kbd "C-q C--") 'hs-toggle-hiding)
(define-key hs-minor-mode-map (kbd "C-q C-^") 'hs-show-all)
(define-key hs-minor-mode-map (kbd "C-q C-¥") 'hs-hide-all)

;;; Profile
(my-require 'elp)

(global-set-key (kbd "C-l C-a C-s") `elp-instrument-package)
(global-set-key (kbd "C-l C-a C-r") `elp-results)
(global-set-key (kbd "C-l C-a C-e") `elp-reset-all)

(defun my-elp-add-my-required-packges ()
  (interactive) 
  (dolist (p my-required-package-list)
    (elp-instrument-package (symbol-name p))))

(setq compilation-auto-jump-to-first-error t)
(setq compilation-scroll-output t)

(my-require 'proof-site)

(my-require 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-maybe-show-regexps nil)
(setq ibuffer-shrink-to-minimum-size t)
(setq ibuffer-use-other-window t)

(my-require 'per-window-point)
(pwp-mode t)

;;; disabled commands
(put 'downcase-region  'disabled nil)   ; Let upcasing work
(put 'erase-buffer     'disabled nil)
(put 'eval-expression  'disabled nil)   ; Let ESC-ESC work
(put 'narrow-to-page   'disabled nil)   ; Let narrowing work
(put 'narrow-to-region 'disabled nil)   ; Let narrowing work
(put 'set-goal-column  'disabled nil)
(put 'upcase-region    'disabled nil)   ; Let downcasing work

(provide 'init-misc)
