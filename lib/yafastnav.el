;;; yafastnav.el -- Yet Another Fast navigation.

(setq yafastnav-regex "\\([a-zA-Z_]+[a-zA-Z0-9_]+\\)")
(setq yafastnav-shortcut-keys
      '(?a ?s ?d ?f ?g ?h ?k ?l
	?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
	?z ?x ?c ?v ?b ?n ?m
	?A ?S ?D ?F ?G ?H ?K ?L
	?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
	?Z ?X ?C ?V ?B ?N ?M
	?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9 ?0
	)
      )

(defun yafastnav-jump-to-current-screen ()
  (interactive)
  (save-excursion
    (move-to-window-line -1)
    (setq bottom (point))
    (move-to-window-line 0)
    (setq top (point))
    )
  (yafastnav-jump-to-between-point top bottom))

(defun yafastnav-jump-to-forward ()
  (interactive)
  (save-excursion
    (setq top (point))
    (move-to-window-line -1)
    (setq bottom (point))
    )
  (yafastnav-jump-to-between-point top bottom))

(defun yafastnav-jump-to-backward ()
  (interactive)
  (save-excursion
    (setq bottom (point))
    (move-to-window-line 0)
    (setq top (point))
    )
  (yafastnav-jump-to-between-point top bottom))


(defun yafastnav-jump-to-between-point (top bottom)
  (save-excursion
    (goto-char top)
    (let ((l nil) (index 0))
      (while (and
	      (re-search-forward yafastnav-regex nil t)
	      (nth index yafastnav-shortcut-keys)
	      (< (point) bottom)
	      )
	(save-excursion
	  (backward-word)
	  (add-to-list 'l (list (nth index yafastnav-shortcut-keys) (point)))

	  (let ((ov (make-overlay (- (point) 1) (point))))
	    (overlay-put ov 'window (selected-window))
	    (overlay-put ov 'before-string (char-to-string (nth index yafastnav-shortcut-keys)))
	    (overlay-put ov 'width 1)
	    (overlay-put ov 'priority 100)
	    (overlay-put ov 'face lazy-highlight-face)
	    ;; (overlay-put ov 'after-string (char-to-string (nth index yafastnav-shortcut-keys)))
	    ;; (overlay-put ov 'after-string (char-to-string (nth index yafastnav-shortcut-keys)))
	    ;; (overlay-put ov 'after-string (nth index yafastnav-shortcut-keys))
	    ;; (overlay-put ov 'display (nth index yafastnav-shortcut-keys))
	    ;; (overlay-put ov 'prefix (nth index yafastnav-shortcut-keys))
	    ;; (overlay-put ov 'postfix (nth index yafastnav-shortcut-keys))
	    )
	  ;; (insert-char (nth index yafastnav-shortcut-keys) 1)
	  
	  (setq index (+ 1 index))))
      (setq ret (assoc (read-char "jump to?:") l))
      ))
  (when ret
	(goto-char (nth 1 ret)))
  (remove-overlays)
  )


(provide 'yafastnav)
