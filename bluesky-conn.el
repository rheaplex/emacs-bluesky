;;; bluesky-conn.el --- Bluesky API connection functions -*- lexical-binding: t; -*-

;; Copyright (c) 2024  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/emacs-bluesky
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file has various methods for calling into bluesky servers according to
;; the atproto protocol.  We use the atproto objects as is, as json objects that
;; use plists and arrays.

(require 'json)
(require 'plz)
(require 'seq)

;;; Code:

(defvar bluesky-session nil
  "The session object for accessing the Bluesky API per host and handle.
This is an alist of host/handle strings and session objects returned by
the Bluesky API.")

(defun bluesky-conn-json-read ()
  "Read JSON from the current buffer and return it in as we expect."
  (let ((json-object-type 'plist))
    (json-read)))

(defun bluesky-conn-json-read-from-string (str)
  "Read JSON from STR and return it as we expect."
  (let ((json-object-type 'plist))
    (json-read-from-string str)))

(defun bluesky-conn-call (host http-method method auth-header on-success on-error &rest args)
  "Call METHOD on the Bluesky instance at HOST.

HTTP-METHOD is the HTTP method to use, such as `get' or `post'.

AUTH-HEADER is the value to use for the bearer authorization header; if
nil it is assumed this is a public endpoint.  ARGS is a plist, but any
values that are nil will be ignored.

ON-SUCCESS is a function that will be called with the response from the
Bluesky API. If ON-SUCCESS is nil, the request will be made
syncronously, and a JSON object will be returned.

ON-ERROR handles all errors."
  (let* ((url (format "https://%s/xrpc/%s%s" host method
                      (if (and args (eq 'get http-method))
                          (concat "?"
                                  (mapconcat (lambda (pair)
                                               (format "%s=%s" (url-hexify-string (substring-no-properties (symbol-name (car pair)) 1))
                                                       (url-hexify-string (format "%s" (cadr pair)))))
                                             (seq-filter (lambda (pair) (not (null (cadr pair))) args)
                                                         (seq-partition args 2))
                                             "&"))
                        "")))
         (args (flatten-list (funcall #'append
                                      (seq-filter (lambda (double)
                                                    (not (null (cadr double))))
                                                  (seq-partition args 2)))))
         (headers (append
                   (when auth-header `(("Authorization" .
                                        ,(format "Bearer %s" auth-header))))
                   '(("Content-Type" . "application/json")))))
    (apply #'plz http-method url :as #'bluesky-conn-json-read
           :headers headers
           (append
            (when (eq http-method 'post) (list :data (json-encode args)))
            (when on-success (list :then on-success))
            (when on-error (list :else
                                 (when on-error
                                   (lambda (resp)
                                     (if-let ((err-resp (plz-error-response resp))
                                              (err (bluesky-conn-json-read-from-string
                                                    err-resp)))
                                         (funcall on-error err)
                                       (error "No error response found in %s" resp))))))))))

(defun bluesky-conn-call-authed (host handle method on-success on-error &rest args)
  "Call METHOD on the Bluesky instance at HOST using HANDLE.
ARGS is a plist, but any values that are nil will be ignored.

ON-SUCCESS is a function that will be called with the response from the
Bluesky API.

ON-ERROR is a function that will be called with an error JSON object.

This function assumes that a session has been created, and will handle
auth refreshes."
  (let ((session (alist-get (format "%s/%s" host handle)
                            bluesky-session
                            nil nil #'equal)))
    (unless session
      (error "Unable to get the Bluesky authentication token, you may need to log in first."))
    (bluesky-conn-call host 'get method (plist-get session :accessJwt)
                       on-success
                       (lambda (err)
                         (if (and auth-header (equal "ExpiredToken" (plist-get err :error)))
                             (progn (bluesky-conn-refresh-session host)
                                    (apply #'bluesky-conn-call host method auth-header args))
                           (when on-error (funcall on-error resp))))
                       args)))

(defun bluesky-conn-create-session (host handle password)
  "Create a session with the Bluesky API at HOST using HANDLE and PASSWORD.
HANDLE is the user's handle on the Bluesky instance at HOST (without any
leading `@'), and PASSWORD is the user's password, or a function that
takes no arguments that produces it. This function will store the
session object in `bluesky-session' for future use, and also return it."
  (let ((result (bluesky-conn-call
                 host 'post "com.atproto.server.createSession" nil nil nil
                 :identifier handle :password (if (functionp password)
                                                  (funcall password)
                                                password))))
    (if result
        (setf (alist-get (format "%s/%s" host handle)
                         bluesky-session
                         nil nil #'equal)
              result)
      (error "Unable to create a session for host %s" host))))

(defun bluesky-conn-refresh-session (host handle)
  "Refresh the session for the Bluesky instance at HOST using HANDLE."
  (let ((session (alist-get (format "%s/%s" host handle)
                            bluesky-session
                            nil nil #'equal)))
    (unless session
      (error "No session found to refresh for host %s" host))
    (setf (alist-get (format "%s/%s" host handle)
                     bluesky-session
                     nil nil #'equal)
          (bluesky-conn-call host 'post "com.atproto.server.refreshSession"
                             (plist-get session :refreshJwt) nil
                             nil))))

(defun bluesky-conn-create-post (host handle collection record)
  "Create a post in the Bluesky instance at HOST using HANDLE.
COLLECTION is the collection to post to, and RECORD is the record, created by
`bluesky-conn-record', to post."
  (bluesky-conn-call-authed
   host handle "com.atproto.server.createPost" nil nil :collection collection :record record))

(defun bluesky-conn-record (text langs facets)
  (append `(:text ,text :createdAt ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
          (when langs `(:langs ,langs))
          (when facets `(:facets ,facets))))

(defun bluesky-get-timeline (host handle &optional cursor limit)
  "Get the timeline for the user HANDLE at HOST.
The CURSOR defines where to start at, and LIMIT is the number of posts
to return."
  (unless (or (null limit) (and (> limit 0) (< limit 100)))
    (error "Number of posts to retrieve must be between 0 and 100"))
  (bluesky-conn-call-authed host handle "app.bsky.feed.getTimeline"
                            nil nil :cursor cursor :limit limit))

(defvar bluesky-conn-cache (make-hash-table :test 'equal)
  "A cache of Bluesky API responses, keyed by URLs.
Anything in here is assumed to be cacheable indefinitely.")

(defun bluesky-conn-get-image-by-url (url)
  "Get an image at URL, using the cache if available."
  (or (gethash url bluesky-conn-cache)
      (puthash url (create-image (plz 'get url :as #'binary) nil 'data) bluesky-conn-cache)))

(defun bluesky-conn-get-blob (host did cid)
  "Get a blob with id CID from account DID."
  (plz 'get (format "https://%s/xrpc/com.atproto.sync.getBlob?did=%s&cid=%s"
                    host (url-hexify-string did) (url-hexify-string cid))
    :as 'binary))

(defun bluesky-conn-get-image-by-ref (host did cid)
  "Get an image with id CID from account DID."
  (create-image (bluesky-conn-get-blob host did cid) nil 'data))

(provide 'bluesky-conn)

;;; bluesky-conn.el ends here
