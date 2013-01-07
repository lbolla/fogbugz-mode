;; org-mode functions for fogbugz

(require 'fogbugz)
(require 'org)

(defun org-insert-todo-fogbugz (case-id)
  "Insert a new heading that uses information from a FogBugz case
with a `case-id'.

Uses the id of the status to determine if this is a TODO or DONE
item.

Inserts the due date as a deadline."
  (let ((buf (current-buffer))
        (fb-case (first (fogbugz-search-cases (number-to-string case-id) '(id title opener-id area-name category-name status-id status-name original-estimate due-date)))))
    (set-buffer buf)
    (org-insert-todo-heading-respect-content)
    (insert (rest (assoc 'title fb-case)))
    (flet ((org-put (property column &optional transformer)
                    (org-entry-put (point) property (if transformer
                                                        (funcall transformer (rest (assoc column fb-case)))
                                                      (rest (assoc column fb-case))))))
      (org-put "CaseId" 'id 'number-to-string)
      (org-put "StatusId" 'status-id 'number-to-string)
      (org-put "Status" 'status-name)
      (org-put "OpenedBy" 'opener-id 'number-to-string)
      (org-put "Area" 'area-name)
      (org-put "Category" 'category-name)
      (org-put "Effort" 'original-estimate)
      (org-put "LastUpdated" 'last-updated-on (lambda (date) (if date (first (split-string date "T" t)) ""))))
    (if (rest (assoc 'due-date fb-case))
        (org-deadline nil (rest (assoc 'due-date fb-case))))
    (let ((status (fogbugz-view-status (rest (assoc 'status-id fb-case)))))
      (when (rest (assoc 'work-done-p status))
        (set-buffer buf)
        (org-todo 'done)))
))

(defun org-fogbugz-tag-alist ()
  "Returns an alist that can be used with `org-tag-alist` or
`org-tag-persistent-alist'. Uses categories and people as
tags. There are no fast-tag-selection characters for any of the
tags."
  (mapcar (lambda (x) (list (replace-regexp-in-string " " "-" (downcase (rest (assoc 'name x))))))
          (concatenate 'list (fogbugz-list-categories) (fogbugz-list-people))))

(defun org-fogbugz-insert-cases (case-ids)
  "Inserts a list of cases using `org-insert-todo-fogbugz'."
  (loop for case-id in case-ids
        do (org-insert-todo-fogbugz case-id)))
