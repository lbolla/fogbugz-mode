;; org-mode functions for fogbugz

(require 'fogbugz)

(defun org-insert-todo-fogbugz (case-id)
  "Insert a new heading that uses information from a FogBugz case
with a `case-id'."
  (org-insert-todo-heading t)
  )

(defun org-fogbugz-tag-alist ()
  "Returns an alist that can be used with `org-tag-alist` or
`org-tag-persistent-alist'. Uses categories and people as
tags. There are no fast-tag-selection characters for any of the
tags."
  (progn
    (fogbugz-logon)
    (mapcar (lambda (x) (list (replace-regexp-in-string " " "-" (downcase (rest (assoc 'name x))))))
            (concatenate 'list (fogbugz-list-categories) (fogbugz-list-people)))))
