;;; init-html.el --- html setting

;; Copyright (C) 2010, 2011 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Keywords: init, html
;; creation time: Wed Apr 28 00:10:36 2010
;;; Commentary:

;;; Code:

(my-require 'html-helper-mode)
(my-require 'init-keybindings)

(add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))
(define-key html-helper-mode-map (kbd "C-m") my-backward-word-command)
(define-key html-helper-mode-map (kbd "C-c C-v") 'browse-url-of-file)

(my-require 'auto-complete)
(add-to-list 'ac-modes 'html-helper-mode)
(add-to-list 'ac-modes 'html-mode)

(defvar html-helper-new-buffer-template
  '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n"
    "<html lang=\"ja\">\n"
    "<head>\n"
    "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=EUC-JP\">\n"
    "<title>" p "</title>\n"
    "</head>\n"
    "<body>\n\n<h1>" p "</h1>\n\n" p
    "\n\n<hr>\n\n<p class=\"noindent\">リンクはご自由にどうぞ。\n\n"
    "<p class=\"noindent\">\n"
    html-helper-timestamp-start
    html-helper-timestamp-end
    "\n</body>\n</html>\n")
  "*Template for new buffers, inserted by html-helper-insert-new-buffer-strings if
html-helper-build-new-buffer is set to t")

(provide 'init-html)
