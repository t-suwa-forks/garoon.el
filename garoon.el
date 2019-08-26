;;; garoon.el --- A Garoon client. -*- lexical-binding: t -*-

;; Copyright (C) 2014 yewton
;; Copyright (C) 2019 Tomotaka SUWA

;; Author: yewton <yewton@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25"))
;; Homepage: https://github.com/t-suwa-forks/garoon.el
;; Keywords: calendar, comm

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A Garron client.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'mm-decode)
(require 'org)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'xml)

;; Customize.

(defgroup garoon nil
  "Access Garoon from Emacs."
  :group 'tools)

(defcustom garoon-wsdl-url
  nil
  "The WSDL url for Garoon soap API."
  :type 'string
  :group 'garoon)

(defcustom garoon-auth-source
  "garoon"
  "The host entry for auth-source."
  :type 'string
  :group 'garoon)

(defcustom garoon-use-basic-auth
  nil
  "Whether enable basic authentication."
  :type 'boolean
  :group 'garoon)

(defcustom garoon-locale
  "ja"
  "Locale for error messages."
  :type 'string
  :group 'garoon)

(defcustom garoon-schedule-org-file
  "~/org/garoon.org"
  "The file storing Garoon events."
  :type 'string
  :group 'garoon)

(defcustom garoon-schedule-fetch-days
  14
  "Max amount of days of period for fetching."
  :type 'integerx
  :group 'garoon)

(defcustom garoon-schedule-use-plan-for-tags
  t
  "Whether set event plan to tags."
  :type 'boolean
  :group 'garoon)

;; Constants.

(defconst garoon-soap-env-ns "http://www.w3.org/2003/05/soap-envelope")
(defconst garoon-soap-addr-ns "http://schemas.xmlsoap.org/ws/2003/03/addressing")
(defconst garoon-soap-sec-ns "http://schemas.xmlsoap.org/ws/2002/12/secext")
(defconst garoon-soap-util-ns "http://schemas.xmlsoap.org/ws/2002/07/utility")

;; Structs.

(cl-defstruct
    (garoon-soap-action
     (:constructor garoon-base-action-new
                   (name parameters &optional (service 'BaseService)))
     (:constructor garoon-schedule-action-new
                   (name parameters &optional (service 'ScheduleService))))
  service
  name
  parameters)

;; XML related functionalities.

(defun garoon-xml-parse (buffer)
  "Return xml from BUFFER."
  (unless buffer
    (error "No data found in buffer %s" buffer))
  (with-current-buffer buffer
    (let ((handle (mm-dissect-buffer 'no-strict))
          xml)
      (with-current-buffer (mm-handle-buffer handle)
        (set-buffer-multibyte t)
        (setq xml (xml-parse-region)))
      (mm-destroy-part handle)
      (unless xml
        (error "Failed to parse xml of buffer %s" buffer))
      (kill-buffer)
      xml)))

(defun garoon-xml-string (node)
  "Make an XML string from NODE."
  (with-temp-buffer
    (save-excursion
      (xml-print (if (symbolp (car node))
                     (list node)
                   node)))
    (flush-lines "^[[:space:]]*$")
    (buffer-string)))

(defmacro garoon-xml-each-node (tag-or-regexp spec &rest body)
  "Evaluate BODY with VAR bound to each child from NODE.
Each child is filtered by TAG-OR-REGEXP.

\(fn TAG-OR-REGEXP (VAR NODE) BODY...)"
  (declare (indent 2)
           (debug (form (symbolp form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (let ((tag (make-symbol "tag")))
    `(let ((,tag ,tag-or-regexp))
       (dolist (,(car spec) (xml-node-children ,(nth 1 spec)))
         (when (and (listp ,(car spec))
                    (cond
                     ((symbolp ,tag)
                      (eq ,tag
                          (xml-node-name ,(car spec))))
                     ((stringp ,tag)
                      (string-match-p
                       ,tag
                       (symbol-name (xml-node-name ,(car spec)))))))
           ,@body)))))

(defmacro garoon-xml-each-path (path spec &rest body)
  "Evaluate BODY with VAR bound to each child of PATH.
PATH is composed of node regexps delimited by \"/\".

\(fn PATH (VAR NODE) BODY...)"
  (declare (indent 2)
           (debug (form (symbolp form) body)))
  (unless (consp spec)
    (signal 'wrong-type-argument (list 'consp spec)))
  (let ((tag-list (make-symbol "tag-list"))
        (node-list (make-symbol "node-list"))
        (temp (make-symbol "temp"))
        (tag (make-symbol "tag")))
    `(let ((,tag-list (split-string ,path "/"))
           (,node-list ,(nth 1 spec)))
       (catch 'not-found
         (unless (listp (car ,node-list))
           (setq ,node-list (list ,node-list)))
         (while ,tag-list
           (let ((,tag (pop ,tag-list))
                 ,temp)
             (while ,node-list
               (garoon-xml-each-node ,tag (,(car spec) (pop ,node-list))
                 (push ,(car spec) ,temp)))
             (unless ,temp
               (throw 'not-found nil))
             (setq ,node-list (nreverse ,temp))))
         (dolist (,(car spec) ,node-list)
           ,@body)))))

;; Date & time utilities.

(defmacro garoon-time-part (time part)
  "Return PART of TIME with `decode-time'.

PART symbols are: :sec :min :hour :day :month :year :dow :dst :utfoff."
  `(pcase (decode-time ,time)
     (`(,sec ,min ,hour ,day ,month ,year ,dow ,dst ,utfoff)
      ;; to suppress flycheck warnings, consume all lexical variables here
      (plist-get
       (list :sec sec
             :min min
             :hour hour
             :day day
             :month month
             :year year
             :dow dow
             :dst dst
             :utfoff utfoff)
       ,part))))

(defun garoon-time-weekday-p (time)
  "Return t if TIME is weekday."
  (<= 1 (garoon-time-part time :dow) 5))

(defun garoon-time-day-equal-p (date1 date2)
  "Return t if DATE1 and DATE2 are same day."
  (and (= (garoon-time-part date1 :year)
          (garoon-time-part date2 :year))
       (= (time-to-day-in-year date1)
          (time-to-day-in-year date2))))

(defun garoon-time-in-week-p (time start)
  "Return t if TIME is in dates between START and START + 6.

Negative START means offset from last day of month."
  (let ((year (garoon-time-part time :year))
        (month (garoon-time-part time :month))
        (day (garoon-time-part time :day)))
    (when (< start 0)
      (setq start (+ (calendar-last-day-of-month month year)
                     start)))
    (<= start day (+ start 6))))

(defun garoon-time-start-of-day (date)
  "Make DATE start."
  (safe-date-to-time (concat date "T00:00:00")))

(defun garoon-time-end-of-day (date)
  "Make DATE end."
  (safe-date-to-time (concat date "T23:59:59")))

(defun garoon-time-add-days (time days)
  "Return time adding DAYS to TIME."
  (time-add time (days-to-time days)))

(defun garoon-time-date-string (time)
  "Format TIME to YYYY-MM-DD."
  (format-time-string "%F" time t))

;; Envelope construction.

(defun garoon-soap-xsd-datetime (time)
  "Make xsd:dateTime string from TIME in UTC."
  (format-time-string "%FT%TZ" time t))

(defun garoon-soap-action-element (action)
  "Return an action element of ACTION."
  `(Action ((xmlns . ,garoon-soap-addr-ns))
           ,(garoon-soap-action-name action)))

(defun garoon-soap-timestamp-element ()
  "Return a timestamp element."
  (let ((created (garoon-soap-xsd-datetime (current-time)))
        (expires (garoon-soap-xsd-datetime (time-add (current-time) 60))))
    `(Timestamp
      ((xmlns . ,garoon-soap-util-ns))
      (Created nil ,created)
      (Expires nil ,expires))))

(defun garoon-soap-locale-element ()
  "Return a locale element."
  `(Locale nil ,garoon-locale))

(defun garoon-soap-security-element ()
  "Return a security element."
  (let ((cred (garoon-soap-credential)))
    `(Security
      ((xmlns . ,garoon-soap-sec-ns)
       (xmlns:wsu . ,garoon-soap-util-ns))
      (UsernameToken
       ((wsu:id . "id"))
       (Username nil ,(pop cred))
       (Password nil ,(pop cred))))))

(defun garoon-soap-header-element (action)
  "Return a header element of ACTION."
  (let (children)
    (push (garoon-soap-locale-element) children)
    (unless garoon-use-basic-auth
      (push (garoon-soap-security-element) children))
    (push (garoon-soap-timestamp-element) children)
    (push (garoon-soap-action-element action) children)
    `(env:Header nil ,@children)))

(defun garoon-soap-body-element (action)
  "Return a body element of ACTION."
  `(env:Body
    nil
    (parameters ,@(garoon-soap-action-parameters action))))

(defun garoon-soap-envelope (action)
  "Return xml for ACTION."
  (let ((xml `(env:Envelope
               ((xmlns:env . ,garoon-soap-env-ns))
               ,(garoon-soap-header-element action)
               ,(garoon-soap-body-element action))))
    (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
            (garoon-xml-string xml))))

;; Service URL functions.

(defvar garoon-soap-service-url-alist nil)

(defun garoon-soap-initialize-service-url-alist ()
  "Initialize `garoon-soap-service-url-alist' from `garoon-wsdl-url'."
  (let ((wsdl (garoon-xml-parse (url-retrieve-synchronously garoon-wsdl-url))))
    (setq garoon-soap-service-url-alist nil)
    (garoon-xml-each-node 'service (service (car wsdl))
      (let ((sym (intern (xml-get-attribute service 'name)))
            (url (catch 'found
                   (garoon-xml-each-path "port/:address$" (addr service)
                     (throw 'found (xml-get-attribute addr 'location))))))
        (when url
          (push (cons sym url) garoon-soap-service-url-alist))))))

(defun garoon-soap-service-url (service)
  "Return url for SERVICE from `garoon-soap-service-url-alist'."
  (unless garoon-soap-service-url-alist
    (garoon-soap-initialize-service-url-alist))
  (cdr (assq service garoon-soap-service-url-alist)))

;; Actions.

(defun garoon-soap-credential ()
  "Return the credential for `garoon-auth-source'."
  (let* ((auth (auth-source-user-and-password garoon-auth-source))
         (username (nth 0 auth))
         (password (nth 1 auth)))
    (unless (and username password)
      (error "No credential found for %s in auth-sources" garoon-auth-source))
    (list username password)))

(defun garoon-soap-basic-auth ()
  "Make a HTTP basic authentication header."
  (let ((cred (string-join (garoon-soap-credential) ":")))
    (cons "Authorization"
          (string-join (list "Basic" (base64-encode-string cred)) " "))))

(defun garoon-soap-request (action)
  "Request ACTION."
  (let ((url (garoon-soap-service-url (garoon-soap-action-service action)))
        (url-request-method "POST")
        (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
        (url-request-extra-headers
         '(("Content-Type" . "application/soap+xml; charset=\"utf-8\"")))
        (url-request-data (garoon-soap-envelope action))
        (url-package-name "garoon.el")
        (returns (format ":%sResponse$/returns"
                         (garoon-soap-action-name action))))
    (when garoon-use-basic-auth
      (push (garoon-soap-basic-auth) url-request-extra-headers))
    (catch 'found
      (let ((response (car (garoon-xml-parse (url-retrieve-synchronously url)))))
        (garoon-xml-each-node ":Body$" (body response)
          (garoon-xml-each-path returns (ret body)
            (throw 'found ret))
          ;; there must be an error response
          (garoon-xml-each-path ":Fault$/:Detail$" (detail body)
            (error "Got an error response:\n%s"
                   (garoon-xml-string detail)))))
      (error "Seems invalid XML response"))))

;; Schedule service APIs.

(defun garoon-schedule-api-get-event-versions (start end)
  "Get schedule versions between START and END."
  (let* ((start-date (garoon-time-date-string start))
         (end-date (garoon-time-date-string end))
         (attrs `((start . ,(concat start-date "T00:00:00Z"))
                  (end . ,(concat end-date "T23:59:59Z"))
                  (start_for_daily . ,start-date)
                  (end_for_daily . ,end-date)))
         (items (org-map-entries
                 (lambda ()
                   `(event_item
                     ((id . ,(org-id-get))
                      (version . ,(org-entry-get nil "VERSION")))))))
         (action (garoon-schedule-action-new "ScheduleGetEventVersions"
                                             (append (list attrs) items)))
         events)
    (garoon-xml-each-node 'event_item (node (garoon-soap-request action))
      (push node events))
    (nreverse events)))

(defun garoon-schedule-api-get-events-by-id (id-list)
  "Get schedules of which id is in ID-LIST."
  (let* ((items (mapcar (lambda (id) `(event_id nil ,id)) id-list))
         (action (garoon-schedule-action-new "ScheduleGetEventsById"
                                             (append '(nil) items)))
         events)
    (when id-list
      (garoon-xml-each-node 'schedule_event (node (garoon-soap-request action))
        (push node events)))
    (nreverse events)))

;; Event parsing functionality.

(defun garoon-event-start-end (start end)
  "Return a pair of time from START and END."
  (if (string-match "T" start)
      (list (safe-date-to-time start)
            (safe-date-to-time end))
    ;; maybe all day
    (list (safe-date-to-time (concat start "T00:00:00JST"))
          nil)))

(defun garoon-event-time (date time)
  "Return a time by parsing combined DATE and TIME."
  (safe-date-to-time (concat (format-time-string "%FT" date) time "JST")))

(defun garoon-event-repeat-day (plist)
  "Make day repeat schedule along with PLIST."
  (let ((start (plist-get plist :start-date))
        (end (plist-get plist :end-date))
        (start_time (plist-get plist :start-time))
        (end_time (plist-get plist :end-time)))
    (list (list (garoon-event-time start start_time)
                (garoon-event-time end end_time)))))

(defun garoon-event-repeat-weekday (plist)
  "Make weekday repeat schedule along with PLIST."
  (let ((temp (plist-get plist :start-date))
        (end (plist-get plist :end-date))
        (start-time (plist-get plist :start-time))
        (end-time (plist-get plist :end-time))
        day1 day2
        result)
    (while (time-less-p temp end)
      (if (garoon-time-weekday-p temp)
          (if day1
              (setq day2 (garoon-event-time temp end-time))
            (setq day1 (garoon-event-time temp start-time)))
        (when (and day1 day2)
          (push (list day1 day2) result))
        (setq day1 nil)
        (setq day2 nil))
      (setq temp (garoon-time-add-days temp 1)))
    result))

(defun garoon-event-repeat-week (plist)
  "Make week repeat schedule along with PLIST."
  (let ((temp (plist-get plist :start-date))
        (end (plist-get plist :end-date))
        (start-time (plist-get plist :start-time))
        (end-time (plist-get plist :end-time))
        (day-of-week (plist-get plist :week))
        result)
    (while (time-less-p temp end)
      (when (= day-of-week (garoon-time-part temp :dow))
        (push (list (garoon-event-time temp start-time)
                    (garoon-event-time temp end-time))
              result))
      (setq temp (garoon-time-add-days temp 1)))
    result))

(defun garoon-event-repeat-month-week (plist)
  "Make month week repeat schedule along with PLIST."
  (let ((temp (plist-get plist :start-date))
        (end (plist-get plist :end-date))
        (start-time (plist-get plist :start-time))
        (end-time (plist-get plist :end-time))
        (day-of-week (plist-get plist :week))
        (week (cdr (assoc-string (plist-get plist :type)
                                 '(("1stweek" . 1)
                                   ("2ndweek" . 8)
                                   ("3rdweek" . 15)
                                   ("4thweek" . 22)
                                   ("lastweek" . -6)))))
        result)
    (while (time-less-p temp end)
      (when (and (= day-of-week (garoon-time-part temp :dow))
                 (garoon-time-in-week-p temp week))
        (push (list (garoon-event-time temp start-time)
                    (garoon-event-time temp end-time))
              result))
      (setq temp (garoon-time-add-days temp 1)))
    result))

(defun garoon-event-repeat-month (plist)
  "Make month repeat schedule along with PLIST."
  (let ((temp (plist-get plist :start-date))
        (end (plist-get plist :end-date))
        (day (plist-get plist :day))
        result)
    (while (time-less-p temp end)
      (when (= day (garoon-time-part temp :day))
        (push (list temp nil) result))
      (setq temp (garoon-time-add-days temp 1)))
    result))

(defun garoon-event-condition (condition)
  "Make CONDITION into plist."
  (let* ((attr (xml-node-attributes condition))
         (plist (list :type (alist-get 'type attr)
                      :day (string-to-number (alist-get 'day attr))
                      :week (string-to-number (alist-get 'week attr))
                      :start-date (garoon-time-start-of-day
                                   (alist-get 'start_date attr))
                      :end-date (alist-get 'end_date attr)
                      :start-time (alist-get 'start_time attr)
                      :end-time (alist-get 'end_time attr)))
         (start-date (plist-get plist :start-date))
         (end-date (plist-get plist :end-date)))
    (unless end-date
      (setq end-date (garoon-time-add-days start-date garoon-schedule-fetch-days))
      (setq end-date (garoon-time-date-string end-date)))
    (plist-put plist :end-date (garoon-time-end-of-day end-date))))

(defun garoon-event-repeat-info (info)
  "Make a list of dates from INFO."
  (let (exclusion events)
    (garoon-xml-each-path "exclusive_datetimes/exclusive_datetime" (ex info)
      (let ((start (safe-date-to-time (xml-get-attribute ex 'start)))
            (end (safe-date-to-time (xml-get-attribute ex 'end))))
        (push (list start end) exclusion)))
    (garoon-xml-each-node 'condition (condition info)
      (let ((plist (garoon-event-condition condition)))
        (setq events
              (pcase (plist-get plist :type)
                ("day"
                 (garoon-event-repeat-day plist))
                ("weekday"
                 (garoon-event-repeat-weekday plist))
                ("week"
                 (garoon-event-repeat-week plist))
                ((pred (string-match-p "[^w]+week$"))
                 (garoon-event-repeat-month-week plist))
                ("month"
                 (garoon-event-repeat-month plist))))))
    (seq-filter (lambda (x)
                  (let ((start (pop x))
                        (end (pop x)))
                    (not (catch 'exclude
                           (dolist (ex exclusion)
                             (and (time-less-p (pop ex) start)
                                  (time-less-p end (pop ex))
                                  (throw 'exclude t)))))))
                events)))

(defun garoon-event-timestamps (event)
  "Return time information of EVENT at DATETIME."
  (let (dates)
    (garoon-xml-each-path "when/date" (date event)
      (let ((start (xml-get-attribute date 'start))
            (end (xml-get-attribute date 'end)))
        (push (garoon-event-start-end start end) dates)))
    (garoon-xml-each-node 'repeat_info (info event)
      (setq dates (garoon-event-repeat-info info)))
    (nreverse dates)))

(defun garoon-event-users (event)
  "Return users of EVENT."
  (let (result)
    (garoon-xml-each-path "members/member/user" (user event)
      (push (xml-get-attribute user 'name) result))
    (nreverse result)))

(defun garoon-event-facilities (event)
  "Return facilities of EVENT."
  (let (result)
    (garoon-xml-each-path "members/member/facility" (facility event)
      (push (xml-get-attribute facility 'name) result))
    (nreverse result)))

;; Org related functions.

(defun garoon-org-goto (id)
  "Return schedule entry of ID from EVENTS."
  (let ((pos (car (org-map-entries '(point) (format "ID=\"%s\"" id)))))
    (when pos
      (goto-char pos))))

(defun garoon-org-timestamp (list)
  "Make org timestamp from LIST.

The LIST forms (START END)."
  (let ((date (car org-time-stamp-formats))
        (time (cdr org-time-stamp-formats))
        (start (pop list))
        (end (pop list)))
    (cond
     ((null end)                        ; all day
      (format-time-string date start))
     ((garoon-time-day-equal-p start end) ; same day
      (replace-regexp-in-string
       ">"
       (format-time-string "--%H:%M>" end)
       (format-time-string time start)))
     (t                                 ; period
      (concat
       (format-time-string time start)
       "--"
       (format-time-string time end))))))

(defun garoon-org-expiration-date (dates)
  "Get max date in DATES."
  (let ((max '(0 0)))
    (dolist (d dates)
      (let ((end (nth 1 d)))
        (if (time-less-p max end)
            (setq max end))))
    (garoon-time-date-string max)))

(defun garoon-org-put-properties (event)
  "Set properties of current item from EVENT."
  (let* ((timestamps (garoon-event-timestamps event))
         (props `((ID
                   . ,(xml-get-attribute event 'id))
                  (VERSION
                   . ,(xml-get-attribute event 'version))
                  (PLAN
                   . ,(xml-get-attribute event 'plan))
                  (DESCRIPTION
                   . ,(xml-get-attribute event 'description))
                  (TIMESTAMPS
                   . ,(string-join
                       (mapcar 'garoon-org-timestamp timestamps) ","))
                  (EXPIRATION
                   . ,(garoon-org-expiration-date timestamps))
                  (USERS
                   . ,(string-join (garoon-event-users event) ","))
                  (FACILITIES
                   . ,(string-join (garoon-event-facilities event) ",")))))
    (when garoon-schedule-use-plan-for-tags
      (org-set-tags (alist-get 'PLAN props)))
    (dolist (prop props)
      (org-entry-put nil (symbol-name (car prop)) (cdr prop)))
    (goto-char (org-entry-end-position))
    (newline)))

;; Scheudle related internal functions.

(defun garoon-schedule--get (id events)
  "Return schedule entry of ID from EVENTS."
  (catch 'found
    (dolist (event events)
      (if (string= id (xml-get-attribute event 'id))
          (throw 'found event)))))

(defun garoon-schedule--add (added events)
  "Add newly ADDED entries to current buffer from EVENTS."
  (while added
    (let* ((id (pop added))
           (event (garoon-schedule--get id events)))
      (goto-char (point-max))
      (org-insert-heading)
      (insert (xml-get-attribute event 'detail))
      (garoon-org-put-properties event))))

(defun garoon-schedule--update (updated events)
  "Update existing UPDATED entries in current buffer from EVENTS."
  (while updated
    (save-excursion
      (let* ((id (pop updated))
             (event (garoon-schedule--get id events)))
        (when (garoon-org-goto id)
          (org-edit-headline (xml-get-attribute event 'detail))
          (garoon-org-put-properties event))))))

(defun garoon-schedule--remove (removed)
  "Remove recently REMOVED entries from current buffer."
  (while removed
    (save-excursion
      (when (garoon-org-goto (pop removed))
        (let ((dates (org-entry-get nil "TIMESTAMPS")))
          (org-entry-put nil "REMOVED" "")
          (org-entry-put nil "TIMESTAMPS"
                         (replace-regexp-in-string
                          "<" "[" (replace-regexp-in-string
                                   ">" "]" dates))))))))

(defun garoon-schedule--archive ()
  "Archive old schedule entries from current buffer."
  (let ((today (current-time)))
    (org-map-entries
     (lambda ()
       (let ((end (concat (org-entry-get nil "EXPIRATION")
                          "T23:59:59JST")))
         (when (time-less-p (safe-date-to-time end) today)
           (org-archive-subtree)
           (setq org-map-continue-from (point))))))))

(defun garoon-schedule--validate-configuration ()
  "Validate configuration."
  (dolist (var '(garoon-wsdl-url
                 garoon-auth-source
                 garoon-schedule-org-file))
    (unless (symbol-value var)
      (user-error "Please specify `%s'" var))))

(defun garoon-schedule--get-diffs ()
  "Get schedule differences."
  (let ((start (current-time))
        (end (garoon-time-add-days
              (current-time) garoon-schedule-fetch-days))
        added modified removed)
    (dolist (event (garoon-schedule-api-get-event-versions start end))
      (let ((id (xml-get-attribute event 'id))
            (operation (xml-get-attribute event 'operation)))
        (push id (pcase operation
                   ("add" added)
                   ("modify" modified)
                   ("remove" removed)))))
    (list added modified removed)))

;; Interactive functions.

;;;###autoload
(defun garoon-schedule-sync ()
  "Synchronize garoon schedule."
  (interactive)
  (garoon-schedule--validate-configuration)
  (let ((buffer (find-file-noselect garoon-schedule-org-file)))
    (with-current-buffer buffer
      (org-mode)
      (pcase (garoon-schedule--get-diffs)
        (`(,added ,modified ,removed)
         (let ((events (garoon-schedule-api-get-events-by-id
                        (append added modified))))
           (garoon-schedule--add added events)
           (garoon-schedule--update modified events)
           (garoon-schedule--remove removed))))
      (garoon-schedule--archive)
      (save-buffer)))
  (message "garoon schedule synchronized."))

(provide 'garoon)

;;; garoon.el ends here
