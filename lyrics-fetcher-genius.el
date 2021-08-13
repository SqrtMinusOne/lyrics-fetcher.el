;;; lyrics-fetcher-genius.el --- Fetch lyrics from genius.com -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel
;; Copyright (C) 2021 Syohei YOSHIDA
;; Copyright (C) 2014-2021 Free Software Foundation, Inc.

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/lyrics-fetcher.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Fetch song lyrics from genius.com.

;;; Code:
(require 'request)
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'shr)
(require 'f)
(require 'dom)

(defcustom lyrics-fetcher-genius-access-token nil
  "Genius access token.  Get one at https://genius.com."
  :type '(string nil)
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-genius-strip-parens-from-query t
  "Strip parens from the query.

I've noticed that these often break the search, e.g. when
searching \"Song (feat. Artist)\""
  :type 'boolean
  :group 'lyrics-fetcher)

(defun lyrics-fetcher-genius-do-search (track callback &optional sync edit)
  "Perform a lyrics search on 'genius.com'.

Requires `lyrics-fetcher-genius-access-token' to be set.

The flow is as follows:
1. Send a GET /search request with a text query
2. Pick the first result (or prompt user if SYNC is non-nil)
3. Fetch lyrics from the HTML page of the result
4. Call CALLBACK with the resulting lyrics string

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher-genius--format-query'.  If the search is
successful, CALLBACK will be called with the result.

If SYNC is non-nil, perform request synchronously and ask the
user to pick the matching search result.

When EDIT is non-nil, edit the query in minibuffer before search.
Genius usually struggles to find song if there is extra
information in the title."
  (lyrics-fetcher-genius--do-query
   track
   (lambda (data)
     (lyrics-fetcher-genius--fetch-lyrics
      (lyrics-fetcher-genius--get-data-from-response data 'url sync)
      callback
      sync))
   sync
   edit))

(defun lyrics-fetcher-genius--do-query (track callback &optional sync edit)
  "Perform a song search on genius.com.

Requires `lyrics-fetcher-genius-access-token' to be set.

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher-genius--format-query'.  If the search is
successful, CALLBACK will be called with the result.

SYNC determines whether the request is synchronous.  The parameter
is useful when it is necessary to ask the user for something right
after the request.

When EDIT is non-nil, edit the query in minibuffer before search."
  (when (string-empty-p lyrics-fetcher-genius-access-token)
    (error "Genius client access token not set!"))
  (message "Sending a query to genius API...")
  (request "https://api.genius.com/search"
    :params `(("q" . ,(lyrics-fetcher-genius--maybe-edit-query
                       (lyrics-fetcher-genius--format-query track)
                       edit))
              ("access_token" . ,lyrics-fetcher-genius-access-token))
    :parser 'json-read
    :sync sync
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun lyrics-fetcher-genius--maybe-edit-query (query edit)
  "If EDIT is non-nil, edit QUERY in minibuffer."
  (when edit
    (read-from-minibuffer "Query: " query))
  query)

(defun lyrics-fetcher-genius--format-query (track)
  "Format track to genius.com query.

When `lyrics-fetcher-genius-strip-parens-from-query' is non-nil,
remove all the text in parens from the query,
for instance (feat.  someone).

TRACK should either be a string or an EMMS-compatible alist, which
contains `info-albumartist' or `info-artist' and `info-title'"
  (if (stringp track)
      track
    (let ((query (concat
                  (or (cdr (assoc 'info-albumartist track))
                      (cdr (assoc 'info-artist track))
                      "")
                  " "
                  (cdr (assoc 'info-title track)))))
      (when lyrics-fetcher-genius-strip-parens-from-query
        (setq query (replace-regexp-in-string
                     (rx (or (: "(" (* nonl) ")")
                             (: "[" (* nonl) "]")))
                     "" query)))
      query)))

(defun lyrics-fetcher-genius--format-song-title (entry)
  "Convert a Genius search ENTRY to a string, which can be used in selection."
  (let ((result (assoc 'result entry)))
    (format "%-40s [lyrics: %s]"
            (cdr (assoc 'full_title result))
            (cdr (assoc 'lyrics_state result)))))

(defun lyrics-fetcher-genius--get-data-from-response (data key &optional ask)
  "Retrieve a song KEY from the Genius response DATA.

If ASK is non-nil, prompt the user for a choice, otherwise select the
first song."
  (when (not (= (cdr (assoc 'status (assoc 'meta data))) 200))
    (error "Error: %s" (cdr (assoc 'message (assoc 'meta data)))))
  (let* ((results (cdr (assoc 'hits (assoc 'response data))))
         (results-songs (seq-filter
                         (lambda (entry)
                           (string-equal (cdr (assoc 'type entry)) "song"))
                         results)))
    (when (seq-empty-p results-songs)
      (error "Error: no results!"))
    (cdr
     (if ask
         (let ((results-songs-for-select
                (mapcar
                 (lambda (entry)
                   (cons (lyrics-fetcher-genius--format-song-title entry)
                         (assoc key (assoc 'result entry))))
                 results-songs)))
           (cdr
            (assoc
             (completing-read
              "Pick a result: "
              results-songs-for-select
              nil t)
             results-songs-for-select)))
       (assoc key (assoc 'result (car results-songs)))))))

(defun lyrics-fetcher-genius--dom-print (dom &optional pretty xml)
  "Print DOM at point as HTML/XML.

If PRETTY, indent the HTML/XML logically.  If XML, generate XML
instead of HTML.

This function was copied from Emacs 28.1 to ensure the backward
compatibilty with Emacs 27."
  (let ((column (current-column)))
    (insert (format "<%s" (dom-tag dom)))
    (let ((attr (dom-attributes dom)))
      (dolist (elem attr)
        (if (and (memq (car elem)
                       '(async autofocus autoplay checked
                               contenteditable controls default
                               defer disabled formNoValidate frameborder
                               hidden ismap itemscope loop
                               multiple muted nomodule novalidate open
                               readonly required reversed
                               scoped selected typemustmatch))
                 (cdr elem)
                 (not xml))
            (insert (format " %s" (car elem)))
          (insert (format " %s=%S" (car elem) (cdr elem))))))
    (let* ((children (dom-children dom))
           (non-text nil))
      (if (null children)
          (insert " />")
        (insert ">")
        (dolist (child children)
          (if (stringp child)
              (insert child)
            (setq non-text t)
            (when pretty
              (insert "\n" (make-string (+ column 2) ? )))
            (lyrics-fetcher-genius--dom-print child pretty xml)))
        (when (and pretty
                   (or (bolp)
                       non-text))
          (unless (bolp)
            (insert "\n"))
          (insert (make-string column ? )))
        (insert (format "</%s>" (dom-tag dom)))))))

(defun lyrics-fetcher-genius--fetch-lyrics (url callback &optional sync)
  "Fetch lyrics from genius.com page at URL and call CALLBACK with the result.

If SYNC is non-nil, the request will be performed synchronously."
  (message "Getting lyrics from %s" url)
  (request url
    :parser 'buffer-string
    :sync sync
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((html (with-temp-buffer
                               (insert data)
                               (libxml-parse-html-region (point-min) (point-max))))
                       (lyrics-div (dom-by-class html (rx bos "lyrics" eos))))
                  (with-temp-buffer
                    (lyrics-fetcher-genius--dom-print lyrics-div)
                    (shr-render-region (point-min) (point-max))
                    (funcall callback
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max)))))))
    :error
    (cl-function
     (lambda (&key error-thrown &allow-other-keys)
       (message "Error!: %S" error-thrown)))))

(defun lyrics-fetcher-genius-download-cover (track callback folder &optional sync edit)
  "Downloads album cover of TRACK.

Requires `lyrics-fetcher-genius-access-token' to be set and
imagemagick's \"convert\" to be available in PATH.

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher-genius--format-query'.  If the search is successful,
CALLBACK will be called with the resulting filename of the large
cover.

In EMMS, the track contains all possible information about the album,
so a sample track is used instead of an actual album object.

The file will be saved to FOLDER and will be named
\"cover_large.<extension>\".

CALLBACK will be called with a path to the resulting file.

If SYNC is non-nil, the user will be prompted for a matching song.

When EDIT is non-nil, edit the query in minibuffer before search."
  (lyrics-fetcher-genius--do-query
   track
   (lambda (data)
     (lyrics-fetcher-genius--save-album-picture
      (lyrics-fetcher-genius--get-data-from-response data 'id sync)
      callback
      folder))
   sync
   edit))

(defun lyrics-fetcher-genius--save-album-picture (id callback folder)
  "Save an album cover of a song of a given ID.

The file will be saved to FOLDER and will be named
\"cover_large.<extension>\".

CALLBACK is passed to `lyrics-fetcher-genius--save-album-url'."
  (request
    (format "https://api.genius.com/songs/%s" id)
    :parser 'json-read
    :params `(("access_token" . ,lyrics-fetcher-genius-access-token))
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (lyrics-fetcher-genius--save-album-url data callback folder)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun lyrics-fetcher-genius--save-album-url (data callback folder)
  "Save album cover of DATA to FOLDER.

DATA should be a response from GET /songs/:id.  The file will be saved
to FOLDER and will be name \"cover_full.<extension>\".

CALLBACK will be called with the path to the resulting file."
  (when (not (= (cdr (assoc 'status (assoc 'meta data))) 200))
    (error "Error: %s" (cdr (assoc 'message (assoc 'meta data)))))
  (let ((url (cdr
              (assoc 'cover_art_url
                     (assoc 'album
                            (assoc 'song
                                   (assoc 'response data)))))))
    (if (not url)
        (message "Album cover not found")
      (message "Downloading the cover image...")
      (request url
        :encoding 'binary
        :complete
        (cl-function
         (lambda (&key data &allow-other-keys)
           (let ((filename
                  (concat folder "cover_large" (url-file-extension url))))
             (with-temp-file filename
               (toggle-enable-multibyte-characters)
               (set-buffer-file-coding-system 'raw-text)
               (seq-doseq (char data)
                 (insert char)))
             (funcall callback filename))))
        :error
        (cl-function
         (lambda (&key error-thrown &allow-other-keys)
           (message "Error!: %S" error-thrown)))))))

(provide 'lyrics-fetcher-genius)
;;; lyrics-fetcher-genius.el ends here
