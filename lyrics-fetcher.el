;;; lyrics-fetcher-genius.el --- fetch song lyrics -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27") (emms "7"))
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

;; Fetch song lyrics TODO

;;; Code:
(require 'lyrics-fetcher-genius)
(require 'emms)

(defgroup lyrics-fetcher ()
  "TODO Fetch lyrics."
  :link '(url-link :tag "GitHub" "https://github.com/SqrtMinusOne/lyrics-fetcher.el"))

(defcustom lyrics-fetcher-fetch-method
  'lyrics-fetcher-genius-do-search
  "A function to perform fetching.

As for now, only genius is available, but this is a point of
extensibility."
  :type 'function
  :options '(lyrics-fetcher-genius-do-search)
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-current-track-method
  'emms-playlist-current-selected-track
  "A function to get current playing track.

By default uses the current selected track in EMMS playlist."
  :group 'lyrics-fetcher
  :type 'function)

(defcustom lyrics-fetcher-lyrics-folder
  "~/Music/lyrics/"
  "The folder in which lyric files will be stored."
  :group 'lyrics-fetcher
  :type 'string)

(defcustom lyrics-fetcher-lyrics-file-extension
  ".txt"
  "Default extension for lyric files."
  :group 'lyrics-fetcher
  :type 'string)

(defcustom lyrics-fetcher-format-song-name-method
  'lyrics-fetcher-format-song-name
  "A function to format song name to a human-readable format.

Has to receive either a string or EMMS alist.  Take a look at
`lyrics-fetcher-format-song-name' for the default implementation."
  :type 'function
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-format-file-name-method
  'lyrics-fetcher-format-file-name
  "A function to format song name to a valid filename.

Has to receive either a string or EMMS alist.  Take a look at
`lyrics-fetcher-format-file-name' for the default implementation."
  :type 'function
  :group 'lyrics-fetcher)

(defun lyrics-fetcher-format-song-name (track)
  "Format TRACK to a human-readable form.

TRACK should be either a string or EMMS alist."
  (if (stringp track)
      track
    (format "%s - %s"
            (or (cdr (assoc 'info-albumartist track))
                (cdr (assoc 'info-artist track)))
            (cdr (assoc 'info-title track)))))

(defun lyrics-fetcher--prepare-string (string)
  "Prepare a STRING to be saved as a part of filename."
  (replace-regexp-in-string
   (rx (or "<" ">" ":" "\"" "/" "\\" "|" "?" "*"))
   "_"
   string))

(defun lyrics-fetcher-format-file-name (track)
  "Convert TRACK to a vaild filename.

TRACK should be either a string or EMMS alist.

The function has to take into account that:
- Symbols '<', '>', ':', ''', '/', '\\', '|', '?', '*' are not allowed on
  some filesystems
- File name can’t be longer than 256 on some filesystems."
  (if (stringp track)
      (substring
       (lyrics-fetcher--prepare-string track)
       0
       (min (length track) 250))
    (let ((artist (lyrics-fetcher--prepare-string
                   (or (cdr (assoc 'info-albumartist track))
                       (cdr (assoc 'info-artist track)))))
          (title (lyrics-fetcher--prepare-string
                  (cdr (assoc 'info-title track)))))
      (format "%s %s"
              (substring artist 0 (min (length artist) 40))
              (substring title 0 (min (length title) 190))))))

(cl-defun lyrics-fetcher-show-lyrics (&optional track &key suppress-open callback force-fetch sync)
  "Show lyrics for TRACK.

TRACK can be either a string or an EMMS alist.  If TRACK is not
set, for instance when called interactively, then
`lyrics-fetcher-current-track-method' will be used to get the
current playing track.

By default, opens already saved lyrics file if one exists,
otherwise performs fetch according to
`lyrics-fetcher-current-track-method'.  The resulting file will be
saved with a name from `lyrics-fetcher-format-file-name-method'.

If SUPPRESS-OPEN is non-nil, don't pop up a window with lyrics.  This
is useful when performing a mass fetch.

If CALLBACK is non-nil, call it with the resulting filename.

If called with \\[universal-argument] or FORCE-FETCH is non-nil, then
always refetch the lyrics text.

If called with \\[universal-argument] \\[universal-argument] or SYNC
is non-nil, then ask the user to select a matching song.  This may be
useful if there are multiple tracks with similar names, and the top
one isn’t the one required."
  (interactive)
  (when (not track)
    (setq track (funcall lyrics-fetcher-current-track-method)))
  (if (not track)
      (message "Error: no track found!")
    (let ((song-name (funcall lyrics-fetcher-format-song-name-method track))
          (file-name (funcall lyrics-fetcher-format-file-name-method track))
          ;; The function is indented to be called both interactive
          ;; and via recursion with asyncronous callbacks, during with
          ;; `current-prefix-arg' will be unset. So this is necessary
          ;; to pass the behavior down the recursion.
          (force-fetch (or force-fetch (member (prefix-numeric-value current-prefix-arg) '(4 16))))
          (sync (or sync (member (prefix-numeric-value current-prefix-arg) '(16)))))
      (if (and (not force-fetch) (lyrics-fetcher--lyrics-saved-p file-name))
          (progn
            (message "Found fetched lyrics for: %s" song-name)
            (when callback
              (funcall callback file-name))
            (unless suppress-open
              (lyrics-fetcher--open-lyrics file-name track)))
        (funcall
         lyrics-fetcher-fetch-method track
         (lambda (result)
           (lyrics-fetcher--save-lyrics result file-name)
           (unless suppress-open
             (lyrics-fetcher--open-lyrics file-name track))
           (when callback
             (funcall callback file-name)))
         sync)))))

(defun lyrics-fetcher-show-lyrics-query (query)
  "Fetch lyrics from a text QUERY.

QUERY should contain everything required to locate the song,
e.g. \"Queen Show Must Go On\".

See `lyrics-fetcher-show-lyrics' for behavior."
  (interactive "sEnter query: ")
  (lyrics-fetcher-show-lyrics query))

(defun lyrics-fetcher-emms-browser-fetch-at-point ()
  "Fetch data for the current point in EMMS browser.

If the point contains just one song, it will be fetched the usual way
via `lyrics-fetcher-show-lyrics'.  Lyrics will be show upon successful
completion.

If the point contains many songs (e.g. it's an album), the lyrics
will be fetched consequentially for every song.  Note that the
process will be stopped at the first failure.

Behavior of the function is modified by \\[universal-argument]
the same way as `lyrics-fetcher-show-lyrics'."
  (interactive)
  (let ((data (emms-browser-bdata-at-point)))
    (if (not data)
        (message "Nothing is found at point!")
      (if (eq (cdr (assoc 'type data)) 'info-title)
          (lyrics-fetcher-show-lyrics (cdadr (assoc 'data data)))
        (lyrics-fetcher--fetch-many
         (lyrics-fetcher--emms-extract-songs data))))))

(defun lyrics-fetcher--emms-extract-songs (bdata)
  "Extract list song alists from EMMS BDATA at point."
  (if (eq (cdr (assoc 'type bdata)) 'info-title)
      (list (cdadr (assoc 'data bdata)))
    (let ((songs '()))
      (dolist (datum (cdr (assoc 'data bdata)))
        (setq songs (append songs (lyrics-fetcher--emms-extract-songs datum))))
      songs)))

(cl-defun lyrics-fetcher--fetch-many (tracks &optional &key start force-fetch sync)
  "Fetch lyrics for every track in the TRACKS list.

This functions calls itself recursively.  START is an indicator of
position in the list.

FORCE-FETCH and SYNC are passed to `lyrics-fetcher-show-lyrics'."
  (unless start
    (setq start 0))
  (message "Fetching lyrics for %s / %s songs" start (+ start (length tracks)))
  (let ((current-prefix-arg current-prefix-arg)
        (force-fetch (or force-fetch (member (prefix-numeric-value current-prefix-arg) '(4 16))))
        (sync (or sync (member (prefix-numeric-value current-prefix-arg) '(16)))))
    (unless (seq-empty-p tracks)
      (lyrics-fetcher-show-lyrics
       (car tracks)
       :suppress-open t
       :callback
       (lambda (&rest _)
         (lyrics-fetcher--fetch-many
          (cdr tracks)
          :start (+ start 1)
          :force-fetch force-fetch
          :sync sync))))))

(defun lyrics-fetcher--lyrics-saved-p (filename)
  "Check if lyrics for FILENAME are already saved."
  (file-exists-p (lyrics-fetcher--process-filename filename)))

(defun lyrics-fetcher--save-lyrics (text filename)
  "Save TEXT of lyrics in `lyrics-fetcher-lyrics-folder'.

FILENAME shoud be given without extension."
  (unless (file-exists-p lyrics-fetcher-lyrics-folder)
    (make-directory lyrics-fetcher-lyrics-folder))
  (with-temp-file (lyrics-fetcher--process-filename filename)
    (insert text)))

(defun lyrics-fetcher--open-lyrics (filename &optional track)
  "Open lyrics for in FILENAME in `lyrics-fetcher-lyrics-folder'.

TRACK is either a string or EMMS alist."
  (let ((buffer (generate-new-buffer
                 (funcall lyrics-fetcher-format-song-name-method
                          (or track filename)))))
    (with-current-buffer buffer
      (insert-file-contents (lyrics-fetcher--process-filename filename))
      (lyrics-fetcher-view-mode)
      (when track
        (setq-local track track))
      (switch-to-buffer-other-window buffer))))

(defun lyrics-fetcher--process-filename (filename)
  "Add the set folder and extension to FILENAME."
  (concat
   lyrics-fetcher-lyrics-folder
   filename
   lyrics-fetcher-lyrics-file-extension))

(defun lyrics-fetcher--close-lyrics ()
  "Close a window and kill its buffer."
  (interactive)
  (quit-window t))

(defvar lyrics-fetcher-view-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'lyrics-fetcher--close-lyrics)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" 'lyrics-fetcher--close-lyrics))
    keymap)
  "Keymap for `lyrics-fetcher-mode'.")

(define-derived-mode lyrics-fetcher-view-mode read-only-mode "Lyrics view"
  "Major mode for viewing lyrics.

\\{lyrics-fetcher-view-mode-map}")

(provide 'lyrics-fetcher)
;;; lyrics-fetcher.el ends here
