;;; init-misc.el --- misc customize.

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, customize

;;; Commentary:

;;; Code:

;最小の ewm 設定例
;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-layout/raw/master/window-layout.el")
;; (auto-install-from-url "http://github.com/kiwanami/emacs-window-manager/raw/master/ewm.el")
(require 'ewm)
(global-set-key (kbd "M-+") 'ewm:start-management)

(require 'detect-block)
(detect-block t)

;; (auto-install-from-url "http://github.com/fukamachi/dont-type-twice-el/raw/master/dont-type-twice.el")
(require 'dont-type-twice)
(global-dont-type-twice t)

(provide 'init-misc)
