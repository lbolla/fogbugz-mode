;; Creates an HTTP server that responds to FogBugz API commands. Used
;; for testing the fogbugz emacs mode (and any other mode). Created
;; because my internet connection wasn't hooked up for a week after I
;; moved into a new place.
;;
;; Copyright (C) 2012 Rudolf Olah <omouse@gmail.com>
(ql:quickload '(hunchentoot cxml))
(use-package '(cxml))

;; Data structures
(defstruct person
  email
  password
  full-name
  filters)

(defstruct filter
  type
  id
  current-status-p
  description)

;; Data
(defvar *fogbugz-people*
  (list (make-person :email "xxx@example.com"
					 :password "password"
					 :full-name "Example User"
					 :filters (list (make-filter :type "builtin"
												 :id "ez349"
												 :description "My Cases")
									(make-filter :type "saved"
												 :id "304"
												 :description "Cases I should have closed months ago")
									(make-filter :type "shared"
												 :id "98"
												 :description "Customer Service Top 10"
												 :current-status-p t))))
  "List of persons that can login.")

(defvar *fogbugz-logged-in*
  (list)
  "An assoc-list of logged in people. The first element is the token,
  and the second element is the email address.")

(defun get-person (token)
  (find (second (find token *fogbugz-logged-in* :key #'first :test #'string=))
		*fogbugz-people*
		:key #'person-email
		:test #'string=))

;; API functions
(defmacro with-api-output (&body body)
  "Wraps the CXML commands in body in the `CXML:WITH-XML-OUTPUT' and a
<response> element."
  `(with-xml-output (make-string-sink)
	 (with-element "response"
	   ,@body)))

(defun fogbugz-api-version ()
  "Returns the API version of FogBugz"
  (with-api-output
	(with-element "version" (text "8"))
	(with-element "minversion" (text "1"))
	(with-element "url" (text "api.xml?"))))

(defun fogbugz-logon (email password)
  "Fogbugz API logon command"
  (let ((people (loop for person in *fogbugz-people*
				   when (string= (person-email person) email)
				   collect person)))
	(with-api-output
	  (case (length people)
		(0 (with-element "error"
			 (attribute "code" "1")
			 (text "Email or password doesn't match")))
		(1 (if (string= password (person-password (first people)))
			   (let ((token (concatenate 'string "mockfogbugztoken-" email)))
				 (setf *fogbugz-logged-in* (cons (list token email) *fogbugz-logged-in*))
				 (with-element "token" (text token)))
			   (with-element "error"
				 (attribute "code" "1")
				 (text "Email or password doesn't match"))))
		(t (with-element "error"
			 (attribute "code" "2")
			 (with-element "people"
			   (loop for person in people
				  do (with-element "person"
					   (text (person-full-name person)))))))))))

(defun fogbugz-logoff (token)
  (with-api-output
	(if (find token *fogbugz-logged-in* :test #'string= :key #'first)
		(progn
		  (setf *fogbugz-logged-in* (remove token *fogbugz-logged-in*
											:test #'string=
											:key #'first))
		  (with-element "success"))
		(with-element "error"
		  (attribute "code" "3")
		  (text "Not logged on")))))

(defun fogbugz-list-filters (token)
  "FogBugz API List Filters command"
  (with-api-output
	(with-element "filters"
	  (loop for filter in (person-filters (get-person token))
		 do (with-element "filter"
			  (attribute "type" (filter-type filter))
			  (attribute "sFilter" (filter-id filter))
			  (when (filter-current-status-p filter)
				(attribute "status" "current"))
			  (text (filter-description filter)))))))

(defun handle-fogbugz-command (command)
  "Dispatches to the various FogBugz API commands with parameters. If
parameters are missing, returns an error."
  (cond
	((string= command "logon")
	 (fogbugz-logon (hunchentoot:parameter "email")
					(hunchentoot:parameter "password")))
	((null (hunchentoot:parameter "token"))
	 (with-api-output
	   (with-element "error"
		 (attribute "code" "3")
		 (text "Not logged on"))))
	((string= command "logoff")
	 (fogbugz-logoff (hunchentoot:parameter "token")))
	((string= command "listFilters")
	 (fogbugz-list-filters (hunchentoot:parameter "token")))
	(t (with-api-output
		 (with-element "error"
		   (attribute "code" "9000")
		   (text "Command doesn't exist or hasn't been implemented by the Mock FogBugz server."))))))

(hunchentoot:define-easy-handler (api :uri "/api.xml") (cmd)
  (setf (hunchentoot:content-type*) "text/xml")
  (if cmd
	  (handle-fogbugz-command cmd)
	  (fogbugz-api-version)))

(defun start-fogbugz-server (&optional (port 4242))
  "Starts the FogBugz server that mocks API commands."
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
