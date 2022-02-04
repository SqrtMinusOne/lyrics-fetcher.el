;;; lyrics-fetcher-neteasecloud.el --- Fetch lyrics from music.163.com -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel
;; Copyright (C) 2021 Syohei YOSHIDA
;; Copyright (C) 2021 Eli Qian
;; Copyright (C) 2014-2021 Free Software Foundation, Inc.

;; Author: Eli Qian <eli.q.qian@gmail.com>
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

(defcustom lyrics-fetcher-neteasecloud-strip-parens-from-query t
  "Strip parens from the query.

I've noticed that these often break the search, e.g. when
searching \"Song (feat. Artist)\""
  :type 'boolean
  :group 'lyrics-fetcher)

(defun lyrics-fetcher-neteasecloud-do-search (track callback &optional sync edit)
  "Perform a lyrics search on 'music.163.com'.

The flow is as follows:
1. Send a POST /search request with a text query
2. Pick the first result (or prompt user if SYNC is non-nil)
3. Fetch lyrics
4. Call CALLBACK with the resulting lyrics string

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher-neteasecloud--format-query'.  If the search is
successful, CALLBACK will be called with the result.

If SYNC is non-nil, perform request synchronously and ask the
user to pick the matching search result.

When EDIT is non-nil, edit the query in minibuffer before search.
Genius usually struggles to find song if there is extra
information in the title."
  (lyrics-fetcher-neteasecloud--do-query
   track
   (lambda (data)
     (lyrics-fetcher-neteasecloud--fetch-lyrics
      (lyrics-fetcher-neteasecloud--get-song-id data sync)
      callback
      sync))
   sync
   edit))

(defun lyrics-fetcher-neteasecloud--fetch-lyrics (song-id callback &optional sync)
  "Fetch lyrics from 'music.163.com' page at URL and call CALLBACK with the result.

SONG-ID is a sequence of number which indicates a song, it can be
returned by 'lyrics-fetcher-neteasecloud--get-song-id' If SYNC is
non-nil, the request will be performed synchronously."
  (message "Getting lyrics from NeteaseCloud API...")
  (request
    (format "http://music.163.com/api/song/lyric?id=%s&lv=1&kv=1&tv=-1" song-id)
    :parser 'json-read
    :sync sync
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'lyric (alist-get 'lrc data)))))
    :error
    (cl-function
     (lambda (&key error-thrown &allow-other-keys)
       (message "Error!: %S" error-thrown)))))

(defun lyrics-fetcher-neteasecloud--do-query (track callback &optional sync edit)
  "Perform a song search on 'music.163.com'.

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher-neteasecloud--format-query'.  If the search is
successful, CALLBACK will be called with the result.

SYNC determines whether the request is synchronous.  The parameter
is useful when it is necessary to ask the user for something right
after the request.

When EDIT is non-nil, edit the query in minibuffer before search."
  (message "Sending a query to NeteaseCloud API...")
  (request "http://music.163.com/api/search/get/"
    :type "POST"
    :data `(("s" . ,(lyrics-fetcher-neteasecloud--maybe-edit-query
		     (lyrics-fetcher-neteasecloud--format-query track)
		     edit))
	    ("limit" . "10")
	    ("type" . "1")
	    ("offset" . "0"))
    :parser 'json-read
    :sync sync
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (funcall callback data)))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error!: %S" error-thrown)))))

(defun lyrics-fetcher-neteasecloud--maybe-edit-query (query edit)
  "If EDIT is non-nil, edit QUERY in minibuffer."
  (if edit
      (read-from-minibuffer "Query: " query)
    query))

(defun lyrics-fetcher-neteasecloud--format-query (track)
  "Format track to 'music.163.com' query.

When `lyrics-fetcher-neteasecloud-strip-parens-from-query' is non-nil,
remove all the text in parens from the query,
for instance (feat.  someone).

TRACK should either be a string or an EMMS-compatible alist, which
contains `info-artist' or `info-title'"
  (if (stringp track)
      track
    (let ((query (concat
		  (cdr (assoc 'info-title track))
                  " "
		  (cdr (assoc 'info-artist track)))))
      (when lyrics-fetcher-neteasecloud-strip-parens-from-query
        (setq query (replace-regexp-in-string
                     (rx (or (: "(" (* nonl) ")")
                             (: "[" (* nonl) "]")))
                     "" query)))
      query)))

(defun lyrics-fetcher-neteasecloud--get-song-id (data &optional ask)
  "Retrieve a song id from the 'music.163.com' response DATA.

If ASK is non-nil, prompt the user for a choice, otherwise select the
first song."
  (if (/= 200 (alist-get 'code data))
      (error "ERROR: %s" (alist-get 'code data))
    (let* ((results (alist-get 'songs (alist-get 'result data))))
      (if (seq-empty-p results)
	  (error "ERROR: no results!")
	(cdr
	 (if ask
	     (let ((results-songs-for-select
		    (mapcar
		     (lambda (entry)
		       (cons (lyrics-fetcher-neteasecloud--format-song-title entry)
			     (assoc 'id entry)))
		     results)))
	       (cdr
		(assoc
		 (completing-read
		  "Pick a result: "
		  results-songs-for-select
		  nil t)
		 results-songs-for-select)))
	   (assoc 'id (aref results 0))))))))

(defun lyrics-fetcher-neteasecloud--format-song-title (entry)
  "Convert a 'music.163.com' search ENTRY to a string, which can be used in selection."
  (format "%s by %s"
	  (cdr (assoc 'name entry))
	  (cdr (assoc 'name (aref (alist-get 'artists entry) 0)))))

(defun lyrics-fetcher-neteasecloud-format-file-name (track)
  "TRACK should be either a string or EMMS alist.
'Emms' requires lyrics files' name should be the same as their
tracks' name except extensions."
  (if (stringp track)
      (substring
       (lyrics-fetcher--prepare-string track)
       0
       (min (length track) 250))
    (let ((full-name (emms-track-get track 'name)))
      (emms-replace-regexp-in-string
       (concat "\\." (file-name-extension full-name) "\\'")
       ""
       (file-name-nondirectory full-name)))))

(defun lyrics-fetcher-neteasecloud-format-song-name (track)
  "Format TRACK to a human-readable form.

TRACK should be either a string or EMMS alist."
  (if (stringp track)
      track
    (format "%s %s"
	    (cdr (assoc 'info-title track))
            (cdr (assoc 'info-artist track)))))

(provide 'lyrics-fetcher-neteasecloud)
;;; lyrics-fetcher-neteasecloud.el ends here
