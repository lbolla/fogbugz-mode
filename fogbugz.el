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

(defun fogbugz-api-version ()
  "Returns the version of the api as a list of two numbers; the
first number is the major version, the second is the minor
version."
  ;((url-http fogbugz-api-url)
  (list 8 1))

(defun fogbugz-api-version-string ()
  (let ((version (fogbugz-api-version)))
	(format "%d.%d" (first version) (second version))))
