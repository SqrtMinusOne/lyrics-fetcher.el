;;; lyrics-fetcher-genius.el --- fetch lyrics from genius.com -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27") (request "0.3.2"))
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
(require 'json)
(require 'seq)
(require 'shr)

(defcustom lyrics-fetcher-genius-access-token nil
  "Genius access token.  Get one at https://genius.com."
  :type '(string nil)
  :group 'lyrics-fetcher)

(defun lyrics-fetcher-genius-do-search (track callback &optional sync)
  "Perform a lyrics search on 'genius.com'.

Requies `lyrics-fetcher-genius-access-token' to be set.

TRACK should be EMMS-compatible alist or string, take a look at
`lyrics-fetcher--genius-format-query'.  If the search is
successful, CALLBACK will be called with the resulting lyrics
text.

If SYNC is non-nil, perform request synchronously and ask the
user to pick the matching search result."
  (if (string-empty-p lyrics-fetcher-genius-access-token)
      (message "Genius client access token not set!")
    (message "Sending a query to genius API...")
    (request "https://api.genius.com/search"
      :params `(("q" . ,(lyrics-fetcher--genius-format-query track))
                ("access_token" . ,lyrics-fetcher-genius-access-token))
      :parser 'json-read
      :sync sync
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (lyrics-fetcher--genius-fetch-lyrics
                   (lyrics-fetcher--genius-get-url-from-response data sync)
                   callback
                   sync)))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

(defun lyrics-fetcher--genius-format-query (track)
  "Format track to genius.com query.

TRACK should either be a string or an EMMS-compatible alist, which
contains `info-albumartist' or `info-artist' and `info-title'"
  (if (stringp track)
      track
    (concat
     (or (cdr (assoc 'info-albumartist track))
         (cdr (assoc 'info-artist track))
         "")
     " "
     (cdr (assoc 'info-title track)))))

(defun lyrics-fetcher--genius-format-song-title (entry)
  "Convert a Genius search ENTRY to a string, which can be used in selection."
  (let ((result (assoc 'result entry)))
    (format "%-40s [lyrics: %s]"
            (cdr (assoc 'full_title result))
            (cdr (assoc 'lyrics_state result)))))

(defun lyrics-fetcher--genius-get-url-from-response (data &optional ask)
  "Retrive a song URL from the Genius response DATA.

If ASK is non-nil, prompt user for a choice, otherwise select the
first song."
  (if (not (= (cdr (assoc 'status (assoc 'meta data))) 200))
      (message "Error: %" (cdr (assoc 'message (assoc 'meta data))))
    (let* ((results (cdr (assoc 'hits (assoc 'response data))))
           (results-songs (seq-filter
                           (lambda (entry)
                             (string-equal (cdr (assoc 'type entry)) "song"))
                           results)))
      (if (seq-empty-p results-songs)
          (message "Error: no results!")
        (cdr
         (if ask
             (let ((results-songs-for-select
                    (mapcar
                     (lambda (entry)
                       (cons (lyrics-fetcher--genius-format-song-title entry)
                             (assoc 'url (assoc 'result entry))))
                     results-songs)))
               (cdr
                (assoc
                 (completing-read
                  "Pick a result: "
                  results-songs-for-select
                  nil t)
                 results-songs-for-select)))
           (assoc 'url (assoc 'result (car results-songs)))))))))

(defun lyrics-fetcher--genius-fetch-lyrics (url callback &optional sync)
  "Fetch lyrics from genius.com page at URL and call CALLBACK with result.

If SYNC is non-nil, the request will be performed synchronously, but
the function will still make Emacs lags, as HTML parsing is pretty
expensive."
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
                    (dom-print lyrics-div)
                    (shr-render-region (point-min) (point-max))
                    (funcall callback
                             (buffer-substring-no-properties
                              (point-min)
                              (point-max)))))))
    :error
    (cl-function
     (lambda (&key error-thrown &allow-other-keys)
       (message "Error!: %S" error-thrown)))))

(provide 'lyrics-fetcher-genius)
;;; lyrics-fetcher-genius.el ends here
