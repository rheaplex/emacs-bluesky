;;; bluesky-ui.el --- Bluesky UI functions -*- lexical-binding: t; -*-

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
;; This file renders atproto objects.  Rendering is done by inserting directly
;; into the buffer.  Overlays are used to both store the underlying objects when
;; needed, but also to stylize text.

(require 'ewoc)
(require 'bluesky-conn)

;;; Code:

(defgroup bluesky nil
  "Bluesky client for Emacs."
  :group 'applications)

(defcustom bluesky-default-host "https://bluesky.dev"
  "The default host of the Bluesky server."
  :type 'string
  :group 'bluesky)

(defcustom bluesky-image-max-width 250
  "The maximum width of images in the Bluesky UI."
  :type 'integer)

(defvar bluesky-host bluesky-default-host
  "The host of the Bluesky server.")

(defface bluesky-author-name
  '((t :inherit font-lock-keyword-face))
  "Face for author names in Bluesky.")

(defface bluesky-author-handle
  '((t :inherit font-lock-normal-face))
  "Face for author handles in Bluesky.")

(defface bluesky-author-attribute
  '((t :inherit font-lock-comment-face))
  "Face for author attributes in Bluesky.")

(defface bluesky-time
  '((t :inherit font-lock-normal-face))
  "Face for time in Bluesky UI")

(defface bluesky-mention
  '((t :inherit link))
  "Face for mentions in Bluesky.")

(defface bluesky-hashtag
  '((t :inherit link))
  "Face for hashtags in Bluesky.")

(defface bluesky-link
  '((t :inherit link))
  "Face for links in Bluesky.")

(defface bluesky-external-title
  '((t :inherit bold))
  "Face for external website titles.")

(defface bluesky-external-description
  '((t :inherit default))
  "Face for external website descriptions.")

(defun bluesky-ui-render-author (author)
  "Render AUTHOR to the current buffer"
  (insert-image (append
                 (bluesky-conn-get-image (plist-get author :avatar))
                 '(:width (1 . ch) :height (1 . ch) :margin 2)))
  (insert
   (concat
    (propertize (plist-get author :displayName) 'face 'bluesky-author-name)
    " "
    (propertize (concat "@" (plist-get author :handle)) 'face 'bluesky-author-handle)
    " "
    (let* ((viewer (plist-get author :viewer))
           (muted (not (eq (plist-get viewer :muted) :json-false)))
           (blocked (not (eq (plist-get viewer :blockedBy) :json-false)))
           (following (plist-get viewer :following)))
      (if (or muted blocked following)
          (propertize
           (format "[%s]"
                   (string-join (append
                                 (when muted '("Muted"))
                                 (when blocked '("Blocked"))
                                 (when following '("Following")))
                                ", "))
           'face 'bluesky-author-attribute)
        "")))))

(defun bluesky-ui-relative-time (timestr)
  "Transform TIMESTR to a relative time string for showing users.
TIMESTR is a string such as 2024-11-29T22:31:30.465Z."
  (let* ((time (date-to-time timestr))
         (now (current-time))
         (diff (time-subtract now time))
         (diff-seconds (time-to-seconds diff)))
    (cond
     ((< diff-seconds 60) "just now")
     ((< diff-seconds 3600) (format "%d minutes ago" (/ diff-seconds 60)))
     ((< diff-seconds 86400) (format "%d hours ago" (/ diff-seconds 3600)))
     ((< diff-seconds 604800) (format "%d days ago" (/ diff-seconds 86400)))
     (t (format-time-string "%Y-%m-%d" time)))))

(defun bluesky-ui-byte-to-char-offset (text byte-offset)
  "Convert BYTE-OFFSET to a character offset in TEXT."
  ;; Take care of the simple case first, to be fast.
  (if (eq (length text) (string-bytes text))
      byte-offset
    ;; We want to find the char offset that corresponds to the byte offset.
    ;; Doing it linearlly may be too expensive, so let's do a binary search.
    (unless (< byte-offset (string-bytes text))
      (error "Byte offset %d is out of range" byte-offset))
    (let* ((len (length text))
           (start 0)
           (end len)
           (char-offset 0))
      (while (< (+ 1 start) end)
        (let ((mid (/ (+ start end) 2)))
          (if (< (string-bytes (substring text 0 mid)) byte-offset)
              (setq start mid)
            (setq end mid))))
      (+ 1 start))))

(defun bluesky-ui-render-text-with-facets (text facets)
  "Render TEXT with FACETS to the current buffer.
The FACETS is a vector of features, with each feature having a type and
a byte range."
  (let ((start-pos (point)))
    (insert text)
    (dolist (facet (append facets nil))
      (let* ((features (plist-get facet :features))
             (index (plist-get facet :index))
             (start-byte (plist-get index :byteStart))
             (start-end (plist-get index :byteEnd))
             ;; We assume that all features have at least one feature in them
             (feature (aref features 0))
             (type (cadr (split-string (plist-get feature :$type) "#")))
             (overlay (make-overlay (+ start-pos
                                       (bluesky-ui-byte-to-char-offset text start-byte))
                                    (+ start-pos
                                       (bluesky-ui-byte-to-char-offset text start-end)))))
        (overlay-put overlay 'bluesky facet)
        ;; For the time being, we use only the first feature of the facet, since
        ;; if there are more than one, it's unclear how to render them.
        (when (and features (> (length features) 0))
          (let ((feature (aref features 0)))
            (overlay-put overlay 'face (pcase type
                                         ("mention" 'bluesky-mention)
                                         ("hashtag" 'bluesky-hashtag)
                                         ("link" 'bluesky-link)))))))))

(defun bluesky-ui-fetch-image-reference (ref author-did)
  "Return the image object from reference REF.
REF can be a URL or an object.
AUTHOR-DID is the DID of the author potentially uploading the image."
  (if (stringp ref)
      (bluesky-conn-get-image-by-url (plist-get external :thumb))
    (bluesky-conn-get-image-by-ref bluesky-host author-did
                                   (plist-get (plist-get ref :ref)
                                              :$link))))

(defun bluesky-ui-render-external (external author-did)
  "Render EXTERNAL to the current buffer.
AUTHOR-DID is the DID of the author of the post, used to fetch content."
  (let ((pos (point)))
    (insert-image (append
                   (bluesky-ui-fetch-image-reference (plist-get external :thumb)
                                                     author-did)
                   `(:max-width ,bluesky-image-max-width)))
    (insert " "
            (propertize
             (plist-get external :title)
             'face 'bluesky-external-title)
            "\n"
            (propertize
             (plist-get external :description)
             'face 'bluesky-external-description))
    (let ((overlay (make-overlay pos (point))))
      (overlay-put overlay 'bluesky external)))
  (insert "\n"))

(defun bluesky-ui-render-embed (embed author-did)
  "Render EMBED to the current buffer.
AUTHOR-DID is the DID of the author of the post, used to fetch content."
  (let ((images (plist-get embed :images))
        (video (plist-get embed :video)))
    (dolist (image (append images nil))
      (insert-image (append
                     (bluesky-conn-get-image-by-ref
                      bluesky-host
                      author-did
                      (plist-get
                       (plist-get (plist-get image :image) :ref)
                       :$link))
                     `(:max-width ,bluesky-image-max-width))))
    (when-let ((external (plist-get embed :external)))
      (bluesky-ui-render-external external author-did))))

(defun bluesky-ui-render-record (record author-did)
  "Render RECORD to the current buffer."
  (let ((text (plist-get record :text))
        (facets (plist-get record :facets))
        (embed (plist-get record :embed)))
    (bluesky-ui-render-text-with-facets text facets)
    (insert "\n")
    (bluesky-ui-render-embed embed author-did)))

(defun bluesky-ui-render-post (post)
  "Render POST to a string."
  (bluesky-ui-render-author (plist-get post :author))
  (let ((record (plist-get post :record))
        (author-did (plist-get (plist-get post :author) :did)))
    (insert " "
            (propertize (bluesky-ui-relative-time
                         (plist-get record :createdAt))
                        'face 'bluesky-time)
            "\n")
    (bluesky-ui-render-record record author-did)
    (insert "\n"
            (format "%d comments — %d repost — %d quotes — %d likes"
                    (plist-get post :replyCount)
                    (plist-get post :repostCount)
                    (plist-get post :quoteCount)
                    (plist-get post :likeCount))
            "\n")))

(provide 'bluesky-ui)
;;; bluesky-ui.el ends here
