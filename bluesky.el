;;; bluesky.el, a Bluesky client for Emacs -*- lexical-binding: t -*-

;; Copyright (c) 2024  Andrew Hyatt <ahyatt@gmail.com>

;; Author: Andrew Hyatt <ahyatt@gmail.com>
;; Homepage: https://github.com/ahyatt/ekg
;; Package-Requires: ((plz "0.9.0))
;; Keywords: outlines, hypermedia
;; Version: 0.0.0
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
;; bluesky is a client for the Bluesky social network, following the atproto
;; spec.  It should be compatible with any other server following that spec.
;; Emacs is not always so capable in the UI department, but this tries to render
;; everything important as clearly as possible.

(require 'bluesky-conn)
(require 'bluesky-ui)
(require 'ewoc)

(defgroup bluesky nil
  "Bluesky client for Emacs."
  :group 'applications)

(defcustom bluesky-default-host "bsky.social"
  "The default host of the Bluesky server."
  :type 'string
  :group 'bluesky)

(defconst bluesky-timeline-buffer-name "*Bluesky Timeline*"
  "The name of the Bluesky timeline buffer.")

(defvar bluesky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'bluesky-feed-refresh)
    (define-key map "n" #'bluesky-feed-next-post)
    (define-key map "p" #'bluesky-feed-previous-post)
    map)
  "Keymap for Bluesky feed mode.")

(define-derived-mode bluesky-mode special-mode "Bluesky"
  "Major mode for Bluesky buffers consisting of lists of posts."
  (setq truncate-lines t)
  (buffer-disable-undo)
  (visual-line-mode 1))

(defvar-local bluesky-host bluesky-default-host
  "Host used in a particular feed.")

(defvar-local bluesky-feed-session nil
  "The Bluesky feed session associated with the buffer's feed.")

(defvar-local bluesky-feed-reader nil
  "A function to read the next part of the feed.")

(defvar-local bluesky-feed-cursor nil
  "The cursor for the current feed.")

(defvar-local bluesky-feed-refresher nil
  "A function to refresh the feed.")

(defvar-local bluesky-ewoc nil
  "The ewoc for the Bluesky feed.")

(defun bluesky-feed-render (feed)
  "Add to buffer's ewoc the posts in FEED."
  (dolist (post (append (plist-get feed :feed) nil))
    (ewoc-enter-last bluesky-ewoc (plist-get post :post))))

(defun bluesky-feed-refresh ()
  "Refresh the Bluesky feed."
  (interactive)
  (ewoc-delete bluesky-ewoc)
  (let ((feed (funcall bluesky-feed-refresher)))
    (setq bluesky-feed-cursor (plist-get feed :cursor))
    (bluesky-feed-render feed)))

(defun bluesky-feed-extend ()
  "Load the Bluesky feed."
  (interactive)
  (let ((feed (funcall bluesky-feed-reader)))
    (setq bluesky-feed-cursor (plist-get feed :cursor))
    (bluesky-feed-render feed)))

(defun bluesky-connect (&optional username password host)
  "Connect to a Bluesky server.

USERNAME is the username to connect with.  It should not include the @
prefix.  This can be nil, and if so, the user will be found via
`auth-source-search'.  Otherwise, the user will be prompted for the
username.

PASSWORD is the password to connect with.  This can be nil, and if so,
the password will be found via `auth-source-search'.  Otherwise, the
user will be prompted for the password."
  (interactive)
  (let* ((host (or host bluesky-default-host))
         (authinfo (car (auth-source-search :host host)))
         (username (or username
                       (when authinfo
                         (plist-get authinfo :user))
                       (read-string "Username: ")))
         (password (or password
                       (when authinfo
                         (plist-get authinfo :secret))
                       (read-passwd "Password: "))))
    (unless (and username password)
      (error "Username and password are required"))
    (with-current-buffer (get-buffer-create bluesky-timeline-buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (bluesky-mode)
      (setq-local bluesky-host host)
      (setq-local bluesky-feed-session
                  (if-let ((session (bluesky-conn-get-session host username)))
                      session
                    (bluesky-conn-create-session host username password)))
      (let ((handle (plist-get bluesky-feed-session :handle)))
        (setq-local bluesky-feed-reader
                    (lambda () (bluesky-conn-get-timeline
                                host handle :cursor bluesky-feed-cursor)))
        (setq-local bluesky-feed-refresher
                    (lambda () (bluesky-conn-get-timeline host handle)))
        (setq-local bluesky-ewoc (ewoc-create #'bluesky-ui-render-post
                                              (format "Timeline for %s" handle))))
      (bluesky-feed-refresh)
      (display-buffer bluesky-timeline-buffer-name))))

(provide 'bluesky)

;;; bluesky.el ends here
