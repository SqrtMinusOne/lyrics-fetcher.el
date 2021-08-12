;;; lyrics-fetcher.el --- Fetch song lyrics and album covers -*- lexical-binding: t -*-

;; Copyright (C) 2021 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "27") (emms "7.5") (f "0.20.0") (request "0.3.2"))
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

;; A package to fetch song lyrics and album covers, mainly to use with
;; EMMS.  Take a look at the package README.org for more information.

;;; Code:
(require 'lyrics-fetcher-genius)
(require 'f)
(require 'emms)

(defgroup lyrics-fetcher ()
  "Fetch song and album covers."
  :link '(url-link :tag "GitHub" "https://github.com/SqrtMinusOne/lyrics-fetcher.el")
  :group 'multimedia)

(defcustom lyrics-fetcher-fetch-method
  'lyrics-fetcher-genius-do-search
  "A function to perform fetching.

As of now, genius.com is the only one available, but this is a
point of extensibility."
  :type 'function
  :options '(lyrics-fetcher-genius-do-search)
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-current-track-method
  'emms-playlist-current-selected-track
  "A function to get the current playing track.

By default uses the currently selected track in the EMMS playlist.

This function has to return either a string or (recommended) an
EMMS-like alist, which has to have the following fields:
- info-artist or info-albumartist
- info-title"
  :group 'lyrics-fetcher
  :type 'function)

(defcustom lyrics-fetcher-lyrics-folder
  "~/Music/lyrics/"
  "The folder in which lyric files will be stored."
  :group 'lyrics-fetcher
  :type 'string)

(defcustom lyrics-fetcher-lyrics-file-extension
  ".txt"
  "Default extension for the lyric files."
  :group 'lyrics-fetcher
  :type 'string)

(defcustom lyrics-fetcher-format-song-name-method
  #'lyrics-fetcher-format-song-name
  "A function to format song name to a human-readable format.

Has to receive either a string or EMMS alist.  Take a look at
`lyrics-fetcher-format-song-name' for the default implementation."
  :type 'function
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-format-file-name-method
  #'lyrics-fetcher-format-file-name
  "A function to format a song name to a valid filename.

Has to receive either a string or EMMS alist.  Take a look at
`lyrics-fetcher-format-file-name' for the default implementation."
  :type 'function
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-download-cover-method
  #'lyrics-fetcher-genius-download-cover
  "A function to perform downloading album cover.

As of now, genius.com is the only one available."
  :type 'function
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-small-cover-size
  "100x100"
  "Small cover size."
  :type 'string
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-medium-cover-size
  "200x200"
  "Medium cover size."
  :type 'string
  :group 'lyrics-fetcher)

(defvar lyrics-fetcher-current-track-method
  "Current track in the lyrics view buffer")

;;; Actual lyrics fetching
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
  "Prepare a STRING to be saved as a part of a filename."
  (replace-regexp-in-string
   (rx (or "<" ">" ":" "\"" "/" "\\" "|" "?" "*"))
   "_"
   string))

(defun lyrics-fetcher-format-file-name (track)
  "Convert TRACK to a valid filename.

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

;;;###autoload
(cl-defun lyrics-fetcher-show-lyrics (&optional track &key suppress-open suppress-switch callback force-fetch sync edit)
  "Show lyrics for TRACK.

TRACK can be either a string or an EMMS alist.  If TRACK is not
set, for instance when called interactively, then
`lyrics-fetcher-current-track-method' will be used to get the
current playing track.

By default, opens already saved lyrics file if one exists,
otherwise performs fetch according to
`lyrics-fetcher-current-track-method'.  The resulting file will be
saved with a name from `lyrics-fetcher-format-file-name-method'.

Resulting lyric files are saved to the
`lyrics-fetcher-lyrics-folder' and have the
`lyrics-fetcher-lyrics-file-extension' extension

If SUPPRESS-OPEN is non-nil, don't pop up a window with lyrics.  This
is useful when performing a mass fetch.

If SUPPRESS-SWITCH is non-nil, create a buffer with lyrics but
don't switch to it.

If CALLBACK is non-nil, call it with the resulting filename.

If called with \\[universal-argument] or FORCE-FETCH is non-nil, then
always refetch the lyrics text.

If called with \\[universal-argument] \\[universal-argument] or SYNC
is non-nil, then ask the user to select a matching song.  This may be
useful if there are multiple tracks with similar names, and the top
one isn’t the one required.

If called with \\[universal-argument] \\[universal-argument]
\\[universal-argument] or EDIT is non-nil, edit the search query
in minibuffer before sending.  This is helpful when there is
extra information in the song title which prevents the API from
finding the song."
  (interactive)
  (unless track
    (setq track (funcall lyrics-fetcher-current-track-method)))
  (unless track
    (error "Error: no track found!"))
  (let ((song-name (funcall lyrics-fetcher-format-song-name-method track))
        (file-name (funcall lyrics-fetcher-format-file-name-method track))
        ;; The function is indented to be called both interactively
        ;; and via recursion in asyncronous callbacks, during with
        ;; `current-prefix-arg' will be unset. So this is necessary
        ;; to pass the behavior down the recursive calls.
        (force-fetch (or force-fetch (member (prefix-numeric-value current-prefix-arg) '(4 16 64))))
        (sync (or sync (member (prefix-numeric-value current-prefix-arg) '(16 64))))
        (edit (or edit (member (prefix-numeric-value current-prefix-arg) '(64)))))
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
           (lyrics-fetcher--open-lyrics file-name track suppress-switch))
         (when callback
           (funcall callback file-name)))
       sync
       edit))))

;;;###autoload
(defun lyrics-fetcher-show-lyrics-query (query)
  "Fetch lyrics from a text QUERY.

QUERY should contain everything required to locate the song,
e.g. \"Queen Show Must Go On\".

See `lyrics-fetcher-show-lyrics' for behavior."
  (interactive "sEnter query: ")
  (lyrics-fetcher-show-lyrics query))

(cl-defun lyrics-fetcher--fetch-many (tracks &optional &key start force-fetch sync edit)
  "Fetch lyrics for every track in the TRACKS list.

This function calls itself recursively.  START is an indicator of
position in the list.

FORCE-FETCH, SYNC and EDIT are passed to `lyrics-fetcher-show-lyrics'."
  (unless start
    (setq start 0))
  (message "Fetching lyrics for %s / %s songs" start (+ start (length tracks)))
  (let ((force-fetch (or force-fetch (member (prefix-numeric-value current-prefix-arg) '(4 16 64))))
        (sync (or sync (member (prefix-numeric-value current-prefix-arg) '(16 64))))
        (edit (or edit (member (prefix-numeric-value current-prefix-arg) '(64)))))
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
          :sync sync
          :edit edit))))))

;;; EMMS integration

;;;###autoload
(defun lyrics-fetcher-emms-browser-show-at-point ()
  "Fetch data for the current point in the EMMS browser.

If the point contains just one song, it will be fetched the usual way
via `lyrics-fetcher-show-lyrics'.  Lyrics will be shown upon successful
completion.

If the point contains many songs (e.g. it's an album), the lyrics
will be fetched consequentially for every song.  The process stops at
the first failure.

The behavior of the function is modified by \\[universal-argument]
the same way as `lyrics-fetcher-show-lyrics'."
  (interactive)
  (let ((data (emms-browser-bdata-at-point)))
    (if (not data)
        (error "Nothing is found at point!")
      (if (eq (cdr (assoc 'type data)) 'info-title)
          (lyrics-fetcher-show-lyrics (cdadr (assoc 'data data)))
        (lyrics-fetcher--fetch-many
         (lyrics-fetcher--emms-extract-songs data))))))

(defun lyrics-fetcher--emms-extract-songs (bdata)
  "Extract a list song alists from EMMS BDATA at point."
  (if (eq (cdr (assoc 'type bdata)) 'info-title)
      (list (cdadr (assoc 'data bdata)))
    (let ((songs '()))
      (dolist (datum (cdr (assoc 'data bdata)))
        (setq songs (append songs (lyrics-fetcher--emms-extract-songs datum))))
      songs)))

;;;###autoload
(defun lyrics-fetcher-emms-browser-fetch-covers-at-point ()
  "Fetch album covers for the current point in the EMMS browser.

If the point contains multiple albums, the covers will be fetched
consequentially for each album.  The process stops at the first
failure.

Requires imagemagick's \"covert\" to be available in PATH.

This requires songs' directories to be grouped by albums, i.e. one
album per one folder.

The files will be saved to the folder with names like
\"cover_small.jpg\", \"cover_med.jpg\", \"cover_large.jpg\".

You can customize the sizes via the `lyrics-fetcher-small-cover-size'
and `lyrics-fetcher-medium-cover-size' variables.

The behavior of the function is modified by \\[universal-argument] the
same way as `lyrics-fetcher-show-lyrics'."
  (interactive)
  (let ((data (emms-browser-bdata-at-point)))
    (if (not data)
        (error "Nothing is found at point!")
      (lyrics-fetcher--fetch-cover-many
       (lyrics-fetcher--emms-extract-albums data)))))

;;;###autoload
(defun lyrics-fetcher-emms-browser-open-large-cover-at-point ()
  "Open cover_large for the current point in EMMS browser."
  (interactive)
  (let ((tracks (lyrics-fetcher--emms-extract-albums (emms-browser-bdata-at-point))))
    (when (seq-empty-p tracks)
      (error "Nothing is found at point!"))
    (let ((cover-file (lyrics-fetcher--get-cover-in-directory
                       (f-dirname (cdr (assoc 'name (car tracks)))))))
      (if (not cover-file)
          (error "Cover not found")
        (start-process "cover-open" nil
                       "xdg-open" cover-file)))))

(defun lyrics-fetcher--emms-extract-albums (bdata)
  "Extract a list of sample song alists from each album in BDATA.

One sample song is given per each album."
  (cond
   ((eq (cdr (assoc 'type bdata)) 'info-album)
    (list (cdadr (assoc 'data (cdadr (assoc 'data bdata))))))
   ((eq (cdr (assoc 'type bdata)) 'info-title)
    (list (cdadr (assoc 'data bdata))))
   (t (let ((sample-songs '()))
        (dolist (datum (cdr (assoc 'data bdata)))
          (setq sample-songs
                (append sample-songs (lyrics-fetcher--emms-extract-albums datum))))
        sample-songs))))

;;; Operating with lyric files

(defun lyrics-fetcher--lyrics-saved-p (filename)
  "Check if lyrics for FILENAME are already saved."
  (f-exists-p (lyrics-fetcher--process-filename filename)))

(defun lyrics-fetcher--save-lyrics (text filename)
  "Save TEXT of lyrics in `lyrics-fetcher-lyrics-folder'.

FILENAME should be given without extension."
  (unless (f-exists-p lyrics-fetcher-lyrics-folder)
    (f-mkdir lyrics-fetcher-lyrics-folder))
  (f-write text 'utf-8 (lyrics-fetcher--process-filename filename)))

(defun lyrics-fetcher--open-lyrics (filename &optional track no-switch)
  "Open lyrics for in FILENAME in `lyrics-fetcher-lyrics-folder'.

TRACK is either a string or EMMS alist.

When NO-SWITCH is non-nil, don't switch to buffer."
  (let* ((buffer (get-buffer-create
                  (funcall lyrics-fetcher-format-song-name-method
                           (or track filename)))))
    (with-current-buffer buffer
      ;; If the buffer already has this mode, disable read-only-mode
      ;; for the time being.
      (read-only-mode -1)
      (erase-buffer)
      (insert-file-contents (lyrics-fetcher--process-filename filename))
      (lyrics-fetcher-view-mode)
      (when track
        (setq-local lyrics-fetcher-current-track track))
      (unless no-switch
        (switch-to-buffer-other-window buffer)))))

(defun lyrics-fetcher--process-filename (filename)
  "Add the set folder and extension to FILENAME."
  (concat
   lyrics-fetcher-lyrics-folder
   filename
   lyrics-fetcher-lyrics-file-extension))

(defun lyrics-fetcher-view-close-lyrics ()
  "Close a window and kill its buffer."
  (interactive)
  (quit-window t))

(defun lyrics-fetcher-view-update-lyrics ()
  "Refetch lyrics for the current lyrics view buffer.

Behavior of the function is modified by \\[universal-argument]
the same way as `lyrics-fetcher-show-lyrics'."
  (interactive)
  (lyrics-fetcher-show-lyrics
   lyrics-fetcher-current-track
   :force-fetch t
   :suppress-switch t))

(defvar lyrics-fetcher-view-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'lyrics-fetcher-view-close-lyrics)
    (define-key keymap (kbd "r") 'lyrics-fetcher-view-update-lyrics)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" 'lyrics-fetcher-view-close-lyrics
        "r" 'lyrics-fetcher-view-update-lyrics))
    keymap)
  "Keymap for `lyrics-fetcher-mode'.")

(define-derived-mode lyrics-fetcher-view-mode text-mode "Lyrics view"
  "Major mode for viewing lyrics.

\\{lyrics-fetcher-view-mode-map}"
  (read-only-mode 1))

;;; Album cover fetching
(cl-defun lyrics-fetcher--fetch-cover-many (tracks &optional &key start force-fetch sync edit)
  "Fetch album covers for every track in the TRACKS list.

This functions calls itself recursively.  START is an indicator of
position in the list.

FORCE-FETCH, SYNC and EDIT are passed to `lyrics-fetcher--fetch-cover'."
  (unless start
    (setq start 0))
  (message "Fetching covers for %s / %s albums" start (+ start (length tracks)))
  (if (seq-empty-p tracks)
      (message "Done. Refresh EMMS browser to see the result.")
    (let ((force-fetch (or force-fetch (member (prefix-numeric-value current-prefix-arg) '(4 16 64))))
          (sync (or sync (member (prefix-numeric-value current-prefix-arg) '(16 64))))
          (edit (or edit (member (prefix-numeric-value current-prefix-arg) '(64)))))
      (lyrics-fetcher--fetch-cover
       (car tracks)
       :callback
       (lambda (&rest _)
         (lyrics-fetcher--fetch-cover-many
          (cdr tracks)
          :start (+ start 1)
          :force-fetch force-fetch
          :sync sync
          :edit edit))
       :sync sync
       :force-fetch force-fetch
       :edit edit))))

(cl-defun lyrics-fetcher--fetch-cover (track &optional &key callback sync force-fetch edit)
  "Fetch cover for a given TRACK.

Call CALLBACK with the resulting filename of full cover.

If SYNC is non-nil, prompt the user for a matching track.

If FORCE-FETCH is non-nil, always fetch regardless of whether the
file exists.

If EDIT is non-nil, edit the query in minibuffer before search."
  (let ((cover-found (lyrics-fetcher--get-cover-in-directory
                      (f-dirname (cdr (assoc 'name track))))))
    (if (and (not force-fetch) cover-found)
        (progn
          (message "Cover already downloaded")
          (when callback
            (funcall callback cover-found)))
      (funcall lyrics-fetcher-download-cover-method
               track
               (lambda (filename)
                 (lyrics-fetcher--generate-cover-sizes filename)
                 (message "Saved cover for %s"
                          (cdr (assoc 'info-album track)))
                 (when callback
                   (funcall callback filename)))
               (concat (f-dirname (cdr (assoc 'name track))) "/")
               sync
               edit))))

(defun lyrics-fetcher--get-cover-in-directory (dirname)
  "Get a path to the large cover file in DIRNAME if one exists."
  (car (f-entries
        dirname
        (lambda (f)
          (string-match-p
           (rx (* nonl) "cover_large" (* nonl)) f)))))

(defun lyrics-fetcher--generate-cover-sizes (filename)
  "Create small and medium versions of FILENAME.

Requires imagemagick installed."
  (dolist (size `((,lyrics-fetcher-small-cover-size . "cover_small.")
                  (,lyrics-fetcher-medium-cover-size . "cover_med.")))
    (shell-command-to-string
     (format "convert \"%s\" -resize %s^ -gravity Center -extent %s \"%s\""
             filename (car size) (car size)
             (f-join (f-dirname filename) (concat (cdr size) (f-ext filename)))))))

(provide 'lyrics-fetcher)
;;; lyrics-fetcher.el ends here
