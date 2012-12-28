;;; fogbugz.el --- Interface to fogbugz API

;; Copyright (C) 2012 Rudolf Olah

;; Author: Rudolf Olah
;; URL: http://github.com/omouse/fogbugz-emacs/tree/master
;; Version: 0.1
;; Created: 2012-11-09
;; By: Rudolf Olah
;; Keywords: ticketing, bugs, tools

;; FogBugz-Emacs is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.
;;
;; FogBugz-Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with FogBugz-Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this interface, set the url, username and password to your
;; FogBugz instance:
;;
;; (customize 'fogbugz)
;;

;;; Code:
(require 'cl)
(require 'xml)
(require 'url)
(require 'url-http)

(provide 'fogbugz)

(defgroup fogbugz nil
  "Controlling FogBugz through its API from Emacs."
  :prefix "fogbugz-"
  :group 'tools)

(defcustom fogbugz-api-url "http://fogbugz.com/api"
  "The URL to the FogBugz API. Do not include the extension (.xml
or .asp) in the URL. This is added depending on the command"
  :group 'fogbugz
  :type 'string)

(defcustom fogbugz-username "username"
  "Username to access the FogBugz API."
  :group 'fogbugz
  :type 'string)

(defcustom fogbugz-password "password"
  "Password to access the FogBugz API."
  :group 'fogbugz
  :type 'string)

(defvar *fogbugz-api-token*
  nil
  "The token that the FogBugz API returns after logging on. Used
by all commands (other than logon of course).")

(defun fogbugz-get-response-body (&optional buffer)
  "Exract HTTP response body from HTTP response, parse it as XML, and return a XML tree as list.
`buffer' may be a buffer or the name of an existing buffer.
 If `buffer' is omitted, current-buffer is parsed.

Modifed based on identica-mode.el, renamed from `identica-get-response-body'; removed the call to `identica-clean-response-body'."
  (or buffer
      (setq buffer (current-buffer)))
  (set-buffer buffer)
  (set-buffer-multibyte t)
  (let ((start (save-excursion
		 (goto-char (point-min))
		 (and (re-search-forward "<\?xml" (point-max) t)
		      (match-beginning 0)))))
    (and start
         (first (xml-parse-region start (point-max))))))

(defun fogbugz-api-do (command &rest url-args)
  "Connects to the Fogbugz api and issues the command. Returns XML response from the buffer (using `fogbugz-get-response-body') or an error.

Constructs the URL for this using COMMAND and URL-ARGS. URL-ARGS are strings that construct the parameter list.

Examples:
    (fogbugz-api-do \"example\" \"&foo=bar\"
                                \"&alice=bob\")
    (fogbugz-api-do \"another-example\")
"
  (let ((buffer (url-retrieve-synchronously (apply 'concat fogbugz-api-url ".asp?cmd=" command
                                                   "&token=" *fogbugz-api-token*
                                                   url-args))))
    (if buffer
        (fogbugz-get-response-body buffer)
      (error "Some kind of url error occurred"))))

(defun fogbugz-map-response (api-do-args
                             emacs-names
                             api-names
                             xml-root-element
                             xml-child-element)
  "Maps the response from a Fogbugz api call to more lispy
names. The API-DO-ARGS are passed to
`fogbugz-api-do'.

EMACS-NAMES are the lispy property names for each item. API-NAMES
are the corresponding property names in the API response.

XML-ROOT-ELEMENT is the root element wrapping the response, and
XML-CHILD-ELEMENT is each child element in the response to map
over."
  (let ((response (apply 'fogbugz-api-do api-do-args)))
    (mapcar (lambda (node)
              (loop for emacs-name in emacs-names
                    for api-name in api-names
                    collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response
                                                       xml-root-element))
                              xml-child-element))))

(defun fogbugz-api-version ()
  "Returns the version of the api as a list of two numbers; the
first number is the major version, the second is the minor
version."
  (let ((buffer (url-retrieve-synchronously (concat fogbugz-api-url ".xml"))))
    (if buffer
        (fogbugz-get-response-body buffer)
      (error "fogbugz-api-url may be incorrect."))))

(defun fogbugz-api-version-string ()
  (let ((version (fogbugz-api-version)))
    (format "%s.%s"
            (third (nth 3 version))
            (third (nth 5 version)))))

(defun fogbugz-logon ()
  (let* ((response (fogbugz-api-do "logon"
                                   "&email=" (url-hexify-string fogbugz-username)
                                   "&password=" fogbugz-password))
         (token (third (first (xml-get-children response 'token))))
         (error-node (first (xml-get-children response 'error))))
    (cond (token (progn (message "Logged on to Fogbugz...")
                        (setq *fogbugz-api-token* token)))
          ((xml-get-attribute-or-nil error-node 'code) (error (third error-node)))
          (t error "Some other error occurred"))))

(defun fogbugz-logoff ()
  (if *fogbugz-api-token*
      (progn
        (fogbugz-api-do "logoff")
        (setq *fogbugz-api-token* nil))
    (display-warning :fogbugz "Not logged on")))

(defun fogbugz-list-filters ()
  "Returns a list of case filters in the structure:

    ((id (type ...)
         (name ...)
         (sFilter ...))
     ...)
"
  (let ((response (fogbugz-api-do "listFilters")))
    (mapcar (lambda (node) (list (cons 'type (xml-get-attribute node 'type))
                                 (cons 'id (xml-get-attribute node 'sFilter))
                                 (cons 'name (third node))))
            (xml-get-children (first (xml-get-children response 'filters)) 'filter))))

(defun fogbugz-set-current-filter (filter-id)
  "Sets the current filter for Fogbugz"
  (fogbugz-api-do "setCurrentFilter" "&sFilter=" (url-hexify-string filter-id))
  (message "Fogbugz cases filter set to %s" filter-id)
  filter-id)

(defun fogbugz-list-projects (&optional read-and-write-p)
  "Returns a list of Fogbugz projects that you can read cases
  from. If read-and-write-p is set, returns a list of projects
  that you can also create new cases for."
  (let ((response (fogbugz-api-do "listProjects" (and read-and-write-p "&fWrite=1"))))
    (mapcar (lambda (node) (loop for emacs-name in '(id name owner-id owner email phone inbox-p workflow-id deleted-p)
                                 for api-name in '(ixProject sProject ixPersonOwner sPersonOwner sEmail sPhone fInbox ixWorkflow fDeleted)
                                 collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response 'projects)) 'project))))

(defun fogbugz-area-type-symbol (area)
  "Converts the type of an area to a symbol."
  (case (elt (second (assoc 'type area)) 0)
    (?0 'normal)
    (?1 'not-spam)
    (?2 'undecided)
    (?3 'spam)))

(defun fogbugz-list-areas (&optional read-and-write-p)
  "Returns a list of Fogbugz areas that you can read cases
from. If read-and-write-p is set, returns a list of areas that
you can also create new cases for."
  (let ((response (fogbugz-api-do "listAreas" (and read-and-write-p "&fWrite=1"))))
    (mapcar (lambda (node) (loop for emacs-name in '(id name project-id project owner-id owner type)
                                 for api-name in '(ixArea sArea ixProject sProject ixPersonOwner sPersonOwner nType)
                                 collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response 'areas)) 'area))))

(defun fogbugz-list-areas-for-project (project-id &optional read-and-write-p)
  "Returns a list of Fogbugz areas for a particular project."
  (let ((response (fogbugz-api-do "listAreas"
                                  "&ixProject=" project-id
                                  (and read-and-write-p "&fWrite=1"))))
    (mapcar (lambda (node) (loop for emacs-name in '(id name project-id project type)
                                 for api-name in '(ixArea sArea ixProject sProject nType)
                                 collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response 'areas)) 'area))))

(defun fogbugz-list-categories ()
  (let ((response (fogbugz-api-do "listCategories")))
    (mapcar (lambda (node) (loop for emacs-name in '(id name plural status-default-id status-default-active-id schedule-item-p deleted-p order icon-type attachment-icon-id)
                                 for api-name in '(ixCategory sCategory sPlural ixStatusDefault ixStatusDefaultActive fIsScheduleItem fDeleted iOrder nIconType ixAttachementIcon)
                                 collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response 'categories)) 'category))))

(defun fogbugz-list-priorities ()
  ;; TODO
  (let ((response (fogbugz-api-do "listPriorities")))
    response))

(defun fogbugz-list-people ()
  "Returns a list of people registered in Fogbugz.

Note: Fogbugz API says you can include an argument but this isn't
needed, the normal list is enough:

    Arguments: fIncludeNormal=1, fIncludeCommunity=1,
    fIncludeVirtual=1 -- if you don't supply any arguments, the
    API assumes you mean fIncludeNormal=1"
  (let ((response (fogbugz-api-do "listPeople")))
    (mapcar (lambda (node) (loop for emacs-name in '(id name email phone administrator-p community-p virtual-p deleted-p notify-p homepage locale language timezone)
                                 for api-name in '(ixPerson sFullName sEmail sPhone fAdministrator fCommunity fVirtual fDeleted fNotify sHomepage sLocale sLanguage sTimeZoneKey)
                                 collect (cons emacs-name (third (first (xml-get-children node api-name))))))
            (xml-get-children (first (xml-get-children response 'people)) 'person))))

(defun fogbugz-list-cases ()
  "Returns a list of all cases. You probably want to use
`fogbugz-filter-cases' or `fogbugz-search-cases'."
  (let ((response (fogbugz-api-do "search"
