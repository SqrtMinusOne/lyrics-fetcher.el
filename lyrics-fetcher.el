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

(defun lyrics-fetcher-show-lyrics (&optional track)
  "Show lyrics for TRACK.

TRACK can be either a string or an EMMS alist.  If TRACK is not
set, e.g. when called interactively, then
`lyrics-fetcher-current-track-method' will be used to get the
current playing track.

By default, opens already saved lyrics file if one exists,
otherwise performs fetch according to
`lyrics-fetcher-current-track-method'.  The resulting file will be
saved with a name from `lyrics-fetcher-format-file-name-method'.

If called with \\[universal-argument], then ask the user to select a
matching song.  This may be useful if there are multiple tracks with
similar names, and the top one isn’t the one required.

If called with \\[universal-argument] \\[universal-argument],
then also always fetch the lyric text."
  (interactive)
  (when (not track)
    (setq track (funcall lyrics-fetcher-current-track-method)))
  (if (not track)
      (message "Error: no track found!")
    (let ((song-name (funcall lyrics-fetcher-format-song-name-method track))
          (file-name (funcall lyrics-fetcher-format-file-name-method track))
          (sync (member (prefix-numeric-value current-prefix-arg) '(4 16)))
          (force-fetch (member (prefix-numeric-value current-prefix-arg) '(16))))
      (if (and (not force-fetch) (lyrics-fetcher--lyrics-saved-p file-name))
          (lyrics-fetcher--open-lyrics file-name track)
        (funcall
         lyrics-fetcher-fetch-method track
         (lambda (result)
           (lyrics-fetcher--save-lyrics result file-name)
           (lyrics-fetcher--open-lyrics file-name track))
         sync)))))

(defun lyrics-fetcher-show-lyrics-query (query)
  "Fetch lyrics from a text QUERY.

QUERY should contain everything required to locate the song,
e.g. \"Queen Show Must Go On\".

See `lyrics-fetcher-show-lyrics' for behavior."
  (interactive "sEnter query: ")
  (lyrics-fetcher-show-lyrics query))

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
    (when (fboundp 'evil-define-key)
      (evil-define-key 'normal keymap
        "q" 'lyrics-fetcher--close-lyrics))
    keymap)
  "Keymap for `lyrics-fetcher-mode'.")

(define-derived-mode lyrics-fetcher-view-mode read-only-mode "Lyrics view"
  "Major mode for viewing lyrics.

\\{lyrics-fetcher-view-mode-map}")

(provide 'lyrics-fetcher)
;;; lyrics-fetcher.el ends here
