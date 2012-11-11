;; org-mode functions for fogbugz

(defun org-insert-todo-fogbugz (case-id)
  "Insert a new heading that uses information from a FogBugz case
with a `case-id'."
  (org-insert-todo-heading t)
  )
