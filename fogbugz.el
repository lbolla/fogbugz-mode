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

(defcustom fogbugz-api-url "http://fogbugz.com/api.xml"
  "The URL to the FogBugz API."
  :group 'fogbugz
  :type 'string)

(defcustom fogbugz-api-username "username"
  "Username to access the FogBugz API."
  :group 'fogbugz
  :type 'string)

(defcustom fogbugz-api-password "password"
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

(defun fogbugz-api-version ()
  "Returns the version of the api as a list of two numbers; the
first number is the major version, the second is the minor
version."
  (let ((buffer (url-retrieve-synchronously (concat fogbugz-api-url "api.xml"))))
    (if buffer
        (fogbugz-get-response-body buffer)
      (error "fogbugz-api-url may be incorrect."))))

(defun fogbugz-api-version-string ()
  (let ((version (fogbugz-api-version)))
    (format "%s.%s"
            (third (nth 3 version))
            (third (nth 5 version)))))

(defun fogbugz-api-logon ()
  (let ((buffer (url-retrieve-synchronously (concat fogbugz-api-url "api.asp?cmd=logon&email="
                                                    (url-hexify-string fogbugz-api-username)
                                                    "&password=" fogbugz-api-password))))
    (if buffer
        (let* ((response (fogbugz-get-response-body buffer))
               (token (third (first (xml-get-children response 'token))))
               (error-node (first (xml-get-children response 'error))))
          (cond (token (progn (message "Logged on to Fogbugz...")
                              (setq *fogbugz-api-token* token)))
                ((xml-get-attribute-or-nil error-node 'code) (error (third error-node)))
                (t error "Some other error occurred")))
      (error "Some kind of url error occurred"))))

(defun fogbugz-api-logoff ()
  (if *fogbugz-api-token*
      (progn
        (url-retrieve-synchronously (concat fogbugz-api-url "api.asp?cmd=logoff&token=" *fogbugz-api-token*))
        (setq *fogbugz-api-token* nil))
    (display-warning :fogbugz "Not logged on")))
