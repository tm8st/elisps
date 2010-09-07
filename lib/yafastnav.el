;;; yafastnav.el -- Yet Another Fast navigation.

(setq yafastnav-regex "\\([a-zA-Z_]+[a-zA-Z0-9_]+\\)")
(setq yafastnav-shortcut-keys
      '(?a ?s ?d ?f ?g ?h ?k ?l
	?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
	?z ?x ?c ?v ?b ?n ?m
	?A ?S ?D ?F ?G ?H ?K ?L
	?Q ?W ?E ?R ?T ?Y ?U ?I ?O ?P
	?Z ?X ?C ?V ?B ?N ?M
	)
      )

(defun yafastnav-jump-to-current-screen ()
  (interactive)
  (save-excursion
    (move-to-window-line -1)
    (setq bottom (point))
    (move-to-window-line 0)
    (let ((l nil) (index 0))
      (while (and
	      (re-search-forward yafastnav-regex nil t)
	      (nth index yafastnav-shortcut-keys)
	      (< (point) bottom)
	      )
	(save-excursion
	  (backward-word)
	  (add-to-list 'l (list (nth index yafastnav-shortcut-keys) (point)))
	  (insert-char (nth index yafastnav-shortcut-keys) 1)
	  (setq index (+ 1 index))))
      (setq ret (assoc (read-char "jump to?:") l))
      ))
  (when ret
	(goto-char (nth 1 ret)))
  )

(provide 'yafastnav)
