;;; lyrics-fetcher-lrclib.el --- Fetch lyrics from lrclib.net -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

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

;; Fetch song lyrics from lrclib.net

;;; Code:
(require 'request)
(require 'json)

(require 'lyrics-fetcher-genius)

(defvar lyrics-fetcher-proxy nil)

(defcustom lyrics-fetcher-lrclib-api "https://lrclib.net/api"
  "API url for lrclib."
  :type 'string
  :group 'lyrics-fetcher)

(defconst lyrics-fetcher-lrclib-id "WU3dRxuJde0phul-LPSIeiTro-eVCL7J3piIr6w6n2k"
  "Public application ID at lrclib.")

(defconst lyrics-fetcher-lrclib-user-agent
  "lyrics-fetcher 0.3.0 (https://github.com/SqrtMinusOne/lyrics-fetcher.el)"
  "User agent for lrclib requests.")

(defun lyrics-fetcher-lrclib--request (endpoint method params callback
                                                &optional sync)
  "Send a request to lrclib.net.

ENDPOINT is the target endpoint (e.g. \"/v2/search\").  METHOD is
the method (e.g. \"GET\").  PARAMS is the parameters list.  CALLBACK
is called with the results.  If SYNC is non-nil, make the request
syncronous."
  (let ((request-curl-options
         (append request-curl-options
                 (when lyrics-fetcher-proxy
                   `("--proxy" ,lyrics-fetcher-proxy)))))
    (request (concat lyrics-fetcher-lrclib-api endpoint)
      :type method
      :params params
      :parser 'json-read
      :sync sync
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (funcall callback data)))
      :headers `(("User-Agent" . ,lyrics-fetcher-lrclib-user-agent)
                 ("Lrclib-App-Id" . ,lyrics-fetcher-lrclib-id))
      :error (cl-function
              (lambda (&key error-thrown &allow-other-keys)
                (message "Error!: %S" error-thrown))))))

(defun lyrics-fetcher-lrclib--format-query (track)
  "Format TRACK for lrclib query.

TRACK should either be a string or an EMMS-compatible alist, which
contains `info-albumartist' or `info-artist', `info-album', `info-title'."
  (if (stringp track)
      `((q . ,track))
    (unless (alist-get 'info-title track)
      (error "Track has no info-title: %s" track))
    `((track_name . ,(alist-get 'info-title track))
      (artist_name . ,(or (alist-get 'info-albumartist track)
                          (alist-get 'info-artist track)))
      (album . ,(alist-get 'info-album track)))))

(defun lyrics-fetcher-lrclib--pick-song (data sync)
  "Pick a song from DATA.

DATA is lrclib.net's response.

If SYNC is nil, pick the first datum with plain lyrics.  Otherwise,
prompt with user."
  (when (seq-empty-p data)
    (user-error "Lrclib has returned nothing :-("))
  (if (not sync)
      (let ((b (seq-find (lambda (datum) (alist-get 'hasPlainLyrics datum))
                         data)))
        (unless b
          (user-error "Lrclib hasn't found any lyrics :-("))
        b)
    (let* ((coll
            (mapcar
             (lambda (datum)
               (cons
                (format "%-40s [lyrics: %s] (ID: %s)"
                        (concat
                         (when-let (a (alist-get 'artistName datum))
                           (format "%s - " a))
                         (alist-get 'trackName datum)
                         (when-let (a (alist-get 'albumName datum))
                           (format " (%s)" a)))
                        (alist-get 'hasPlainLyrics datum)
                        (alist-get 'id datum))
                datum))
             data)))
      (alist-get (completing-read "Result: " coll) coll
                 nil nil #'equal))))

(defun lyrics-fetcher-lrclib-do-search (track callback &optional sync edit)
  "Perform a lyrics search on lrclib.net.

The flow is as follows:
1. Send a GET /v2/search request with the query
2. Pick the first result (or prompt the user if SYNC is non-nil)
3. Fetch the lyrics
4. Call CALLBACK with the text.

TRACK is either a string or an EMMS-compatible alist, see
`lyrics-fetcher-lrclib--format-query'.

When EDIT is non-nil, edit the query in the minibuffer before search."
  (when edit
    (if (stringp track)
        (setq track (read-from-minibuffer "Edit query: " track))
      (setq track (read
                   (read-from-minibuffer "Edit query (alist): "
                                         (prin1-to-string track))))))
  (lyrics-fetcher-lrclib--request
   "/v2/search" "GET"
   (lyrics-fetcher-lrclib--format-query track)
   (lambda (data)
     (let ((datum (lyrics-fetcher-lrclib--pick-song data sync)))
       (lyrics-fetcher-lrclib--request
        (format "/get/%d" (alist-get 'id datum)) "GET" nil
        (lambda (result)
          (let ((lyrics (alist-get 'plainLyrics result)))
            (unless (stringp lyrics)
              (user-error "The selected song has no lyrics"))
            (funcall callback lyrics))))))
   sync))

(provide 'lyrics-fetcher-lrclib)
;;; lyrics-fetcher-lrclib.el ends here
