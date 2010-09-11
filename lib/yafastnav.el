;;; yafastnav.el --- yet another fastnav

;; Copyright (C) 2010 tm8st

;; Author: tm8st <tm8st@hotmail.co.jp>
;; Version: 0.1
;; Keywords: cursor-move, fastnav
;; creation time: Thu Sep  9 00:34:17 2010

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 表示されているウィンドウ内への移動の高速化のための拡張。
;; 最初に正規表現で候補をリストアップし、ショートカットキーと対応づけ、
;; 次にショートカットキーを入力することで、目的の位置へ移動する。

;; 現状テスト実装のため、C-gでキャンセルした場合にちゃんとoverlayが消えなかったりします。

;; 設定例

;; (require 'yafastnav)
;; (global-set-key (kbd "C-l C-h") 'yafastnav-jump-to-current-screen)
;; (global-set-key (kbd "C-l C-.") 'yafastnav-jump-to-forward)
;; (global-set-key (kbd "C-l C-r") 'yafastnav-jump-to-backward)

;;; Code:

;; variables

(defvar yafastnav-regex "\\([a-zA-Z_?]+[a-zA-Z0-9_-]+\\)"
  "リストアップする要素の指定用正規表現")

(defvar yafastnav-shortcut-keys
     '(
       ?a ?s ?d ?f ?g ?h ?k ?l
       ?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
       ?z ?x ?c ?v ?b ?n ?m
       ?A ?S ?D ?F ?G ?H ?K ?L
       ?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
       ?Z ?X ?C ?V ?B ?N ?M
       ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
       ?, ?. ?; ?: ?- ?^
       ?< ?> ?@ ?\* ?\[ ?\]
       ?\\ ?\  ?' ?( ?) ?=
       ?~ ?| ?{ ?} ?\_
       ?! ?\" ?# ?$ ?% ?&
       )
     "要素の選択用ショートカットキーリスト"
     )

(defface yafastnav-shortcut-key-face
  '((((class color)) (:foreground "LightPink" :background "gray15"))
    (t ()))
  ""
  ;; :group 'howm-faces
  )
(defvar yafastnav-shortcut-key-face 'yafastnav-shortcut-key-face
  "")

;; functions

(defun yafastnav-jump-to-current-screen ()
  "現在の画面内の候補へのジャンプ"
 (interactive)
 (save-excursion
   (move-to-window-line -1)
   (setq bottom (point))
   (move-to-window-line 0)
   (setq top (point))
   )
 (yafastnav-jump-to-between-point top bottom))

(defun yafastnav-jump-to-forward ()
  "現在の画面内のカーソル位置の下の候補へのジャンプ"
 (interactive)
 (save-excursion
   (setq top (point))
   (move-to-window-line -1)
   (setq bottom (point))
   )
 (yafastnav-jump-to-between-point top bottom))

(defun yafastnav-jump-to-backward ()
  "現在の画面内のカーソル位置の上の候補へのジャンプ"
 (interactive)
 (save-excursion
   (setq bottom (point))
   (move-to-window-line 0)
   (setq top (point))
   )
 (yafastnav-jump-to-between-point top bottom))

(defun yafastnav-jump-to-between-point (top bottom)
  "候補の作成とジャンプの実行"
 (let ((ret) (ls nil) (index 0))
   (save-excursion
     (goto-char top)
     (while
	 (and
	  (re-search-forward yafastnav-regex bottom 1)
	  (nth index yafastnav-shortcut-keys)
	  (<= (point) bottom))
       (save-excursion
	 (goto-char (match-beginning 0))
	 (add-to-list 'ls
		      (list
		       (nth index yafastnav-shortcut-keys)
		       (point)))
	 (let ((ov (make-overlay (point) (1+ (point)))))
           (overlay-put ov 'before-string
			(propertize
			 (char-to-string
			  (nth index yafastnav-shortcut-keys))
			 'face yafastnav-shortcut-key-face))
	   ;; 'face lazy-highlight-face))
	   (overlay-put ov 'window (selected-window))
           (overlay-put ov 'width 1)
           (overlay-put ov 'priority 100)
           )
         (setq index (1+ index))))
     (setq ret
      (if (> index 0)
	  (assoc (read-char "jump to?:") ls)
	nil)))
   (if ret
     (goto-char (nth 1 ret))
     (message "none candidate.")))
 (remove-overlays))

(provide 'yafastnav)