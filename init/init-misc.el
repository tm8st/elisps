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

(provide 'init-misc)
