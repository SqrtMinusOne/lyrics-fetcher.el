;;; lyrics-fetcher-analyze.el --- Fetch lyrics from music.163.com -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel
;; Copyright (C) 2022 Eli Qian
;; Copyright (C) 2021 Syohei YOSHIDA

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

;; Perform various interesting computations on lyrics.

;;; Code:
(require 'lyrics-fetcher)
(require 'emms-browser)
(require 'f)

;; XXX This is not `defcustom' because the customize UI doesn't look
;; good with such a large list
(defvar lyrics-fetcher-analyze-stop-words
  '("me" "my" "myself" "we" "our" "ours" "ourselves" "you" "your"
    "yours" "yourself" "yourselves" "he" "him" "his" "himself" "she"
    "her" "hers" "herself" "it" "its" "itself" "they" "them" "their"
    "theirs" "themselves" "what" "which" "who" "whom" "this" "that"
    "these" "those" "am" "is" "are" "was" "were" "be" "been" "being"
    "have" "has" "had" "having" "do" "does" "did" "doing" "a" "an"
    "the" "and" "but" "if" "or" "because" "as" "until" "while" "of"
    "at" "by" "for" "with" "about" "against" "between" "into"
    "through" "during" "before" "after" "above" "below" "to" "from"
    "up" "down" "in" "out" "on" "off" "over" "under" "again" "further"
    "then" "once" "here" "there" "when" "where" "why" "how" "all"
    "any" "both" "each" "few" "more" "most" "other" "some" "such" "no"
    "nor" "not" "only" "own" "same" "so" "than" "too" "very" "s" "t"
    "can" "will" "just" "don" "should" "now" "ve" "chorus" "verse"
    "pre-chorus" "bridge" "instrumental" "interlude" "intro" "outro"
    "every" "ll")
  "List of words to ignore when doing a word count.

Taken from NLTK + some additions of mine.")

(defcustom lyrics-fetcher-analyze-top-n 10
  "Number of top words to display in the analysis."
  :type 'integer
  :group 'lyrics-fetcher)

(defvar lyrics-fetcher-analyze-lyrics-count-buffer-name
  "*Lyrics Fetcher Lyrics Counts*"
  "Name of the buffer to display lyrics counts.")

(defun lyrics-fetcher-analyze--get-bdata ()
  "Get bdata from EMMS browser.

When the region is not active, return the entry at point.  Otherwise,
return all the entries in the region."
  (let ((count
         (if (use-region-p)
             (-
              (line-number-at-pos (region-end))
              (line-number-at-pos (region-beginning)))
           1)))
    (save-excursion
      (when (use-region-p)
        (goto-char (region-beginning)))
      (cl-loop for i from 1 to count
               collect (prog1
                           (emms-browser-bdata-at-point)
                         (forward-line))))))

(defun lyrics-fetcher-analyze--get-lyrics-paths (bdata &optional hash)
  "Recursively get paths to existing files with lyrics.

BDATA is a list of EMMS bdata, such as returned by
`emms-browser-bdata-at-point'.  HASH is a table to store the result,
where the key is the path and the value is the path to the file."
  (unless hash
    (setq hash (make-hash-table :test 'equal)))
  (dolist (bdatum bdata)
    (let ((maybe-track (car-safe (alist-get 'data bdatum))))
      (if (emms-track-p maybe-track)
          (let ((file-name (funcall lyrics-fetcher-format-file-name-method maybe-track)))
            (if (lyrics-fetcher--lyrics-saved-p file-name)
                (puthash (emms-track-name maybe-track)
                         (lyrics-fetcher--process-filename file-name) hash)
              (message "No lyrics fetched for %s"
                       (funcall lyrics-fetcher-format-song-name-method maybe-track))))
        (lyrics-fetcher-analyze--get-lyrics-paths (alist-get 'data bdatum) hash))))
  hash)

(defun lyrics-fetcher-analyze--get-entiries-recursive (bdata level lyrics-paths)
  "Get data for analysis recursively.

BDATA is a list of EMMS bdata.  LEVEL is the level of the recursion.
LYRICS-PATHS is the output of
`lyrics-fetcher-analyze--get-lyrics-paths'."
  (cl-loop for bdatum in bdata
           for maybe-track = (car-safe (alist-get 'data bdatum))
           ;; If the level is 1 or higher and the current item is a
           ;; track, the group is the track and its lyrics
           if (and (>= level 1)
                   (emms-track-p maybe-track)
                   (gethash (emms-track-name maybe-track) lyrics-paths))
           collect `((kind . track)
                     (track . ,maybe-track)
                     (lyrics . (,(f-read (gethash (emms-track-name maybe-track) lyrics-paths)))))
           ;; If the level is 1 and the current item is not a track,
           ;; the group is the item and lyrics of all its tracks
           else if (and (= level 1)
                        (not (emms-track-p maybe-track)))
           collect `((kind . last-item)
                     (item . ,bdatum)
                     (lyrics . ,(lyrics-fetcher-analyze--get-entiries-recursive
                                 (alist-get 'data bdatum)
                                 (1- level)
                                 lyrics-paths)))
           ;; If the level is less than 1, just return the lyrics for
           ;; each track
           else if (and (< level 1)
                        (emms-track-p maybe-track)
                        (gethash (emms-track-name maybe-track) lyrics-paths))
           collect (f-read (gethash (emms-track-name maybe-track) lyrics-paths))
           else if (and (< level 1)
                        (not (emms-track-p maybe-track)))
           append (lyrics-fetcher-analyze--get-entiries-recursive
                   (alist-get 'data bdatum)
                   (1- level)
                   lyrics-paths)
           ;; If the level is higher than 1 and the current item is
           ;; not a track, recursively process groups below
           else if (and (> level 1)
                        (not (emms-track-p maybe-track)))
           collect `((kind . item)
                     (item . ,bdatum)
                     (groups . ,(lyrics-fetcher-analyze--get-entiries-recursive
                                 (alist-get 'data bdatum)
                                 (1- level)
                                 lyrics-paths)))))

(defun lyrics-fetcher-analyze--get-entities ()
  "Get data for the analysis."
  (let* ((tracks (lyrics-fetcher-analyze--get-bdata))
         (level (if current-prefix-arg
                    (truncate (sqrt (prefix-numeric-value current-prefix-arg)))
                  1))
         (lyrics-paths (lyrics-fetcher-analyze--get-lyrics-paths tracks)))
    (lyrics-fetcher-analyze--get-entiries-recursive tracks level lyrics-paths)))

(defun lyrics-fetcher-analyze--get-top-n (texts)
  "Get the top N words from TEXTS.  Return a list of (word . count).

TEXTS is a list of strings."
  (let ((counts (make-hash-table :test 'equal))
        (stop-words (make-hash-table :test 'equal)))
    (cl-loop for word in lyrics-fetcher-analyze-stop-words
             do (puthash word t stop-words))
    (dolist (text texts)
      (dolist (word (split-string text "[^[:word:]]+"))
        (let ((word (downcase word)))
          (when (and (> (length word) 1)
                     (not (gethash word stop-words)))
            (puthash word (1+ (gethash word counts 0)) counts)))))
    (setq counts (cl-loop for word being the hash-keys of counts
                          collect (cons word (gethash word counts 0))))
    (seq-take
     (cl-sort counts '> :key 'cdr)
     lyrics-fetcher-analyze-top-n)))

(defun lyrics-fetcher-analyze--format-top-n (counts)
  "Format the top N words list for display.

COUNTS is a list of (word . count)."
  (mapconcat
   (lambda (count)
     ;; (format "%s: %d" (car count) (cdr count))
     (car count))
   counts
   "; "))

(defun lyrics-fetcher-analyze--lyrics-count-display (data &optional level)
  "Display the lyrics count for DATA.

DATA is a list of entities as returned by
`lyrics-fetcher-analyze--get-entities'.  LEVEL is the level of the
recursion."
  (unless level
    (setq level 0))
  (dolist (datum data)
    (pcase (alist-get 'kind datum)
      ('track
       (let ((name (funcall lyrics-fetcher-format-song-name-method
                            (alist-get 'track datum)))
             (counts (lyrics-fetcher-analyze--get-top-n
                      (alist-get 'lyrics datum))))
         (insert (make-string (* level 2) ?\s))
         (insert (propertize
                  (format "Track: %s\n" name)
                  'face 'emms-browser-track-face))
         (insert (make-string (* level 2) ?\s))
         (insert (lyrics-fetcher-analyze--format-top-n counts))
         (insert "\n")))
      ('last-item
       (let ((name (alist-get 'name (alist-get 'item datum)))
             (counts (lyrics-fetcher-analyze--get-top-n
                      (alist-get 'lyrics datum))))
         (insert (make-string (* level 2) ?\s))
         (insert (propertize
                  (format "%s\n" name)
                  'face 'emms-browser-album-face))
         (insert (make-string level ?\s))
         (insert (lyrics-fetcher-analyze--format-top-n counts))
         (insert "\n")))
      ('item
       (let ((name (alist-get 'name (alist-get 'item datum))))
         (insert (make-string (* level 2) ?\s))
         (insert (propertize
                  (format "%s\n" name)
                  'face 'emms-browser-album-face))
         (lyrics-fetcher-analyze--lyrics-count-display (alist-get 'groups datum)
                                                       (1+ level)))))))

(defvar lyrics-fetcher-analyze-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "q") 'lyrics-fetcher-view-close-lyrics)
    (when (fboundp 'evil-define-key*)
      (evil-define-key* 'normal keymap
        "q" 'lyrics-fetcher-view-close-lyrics))
    keymap)
  "Keymap for `lyrics-fetcher-analyze-mode'.")

(define-derived-mode lyrics-fetcher-analyze-mode special-mode "Lyrics Fetcher Analysis"
  "Major mode for viewing lyrics analysis data.

\\{lyrics-fetcher-analyze-mode-map}")

;;;###autoload
(defun lyrics-fetcher-analyze-lyrics-count ()
  "Count top-N words for EMMS tracks at point or region.

\\[universal-argument] sets the level of detalization.  I.e. in
`emms-browse-by-artist', running this function on an artist results in
per-artist summary, running with one argument creates a summary on
each of the artist's albums, and running with two arguments creates a
summary for each track.

Change the `lyrics-fetcher-analyze-top-n' variable to control the
number of words to count.  You may also want to extend
`lyrics-fetcher-analyze-stop-words' if you're running this with a
language other than English."
  (interactive)
  (let ((data (lyrics-fetcher-analyze--get-entities)))
    (with-current-buffer (get-buffer-create lyrics-fetcher-analyze-lyrics-count-buffer-name)
      (lyrics-fetcher-analyze-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (lyrics-fetcher-analyze--lyrics-count-display data))))
    (switch-to-buffer-other-window lyrics-fetcher-analyze-lyrics-count-buffer-name)))

(provide 'lyrics-fetcher-analyze)
;;; lyrics-fetcher-analyze.el ends here
