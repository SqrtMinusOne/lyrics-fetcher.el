(require 'lyrics-fetcher-genius)


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


(defcustom lyrics-fetcher-lyrics-folder
  "~/Music/lyrics/"
  "The folder in which lyrics will be stored."
  :group 'lyrics-fetcher)

(defcustom lyrics-fetcher-lyrics-file-extension
  ".txt"
  "Default extension for lyric files."
  :group 'lyrics-fetcher)

(defun lyrics-fetcher-do-fetch (query)
  (interactive "sEnter query: ")
  query)

(defun lyrics-fetcher--save-lyrics (text filename)
  "Save TEXT of lyrics in ’lyrics-fetcher-lyrics-folder’.

FILENAME shoud be given without extension."
  (unless (file-exists-p lyrics-fetcher-lyrics-folder)
    (make-directory lyrics-fetcher-lyrics-folder))
  (with-temp-file (lyrics-fetcher--process-filename filename)
    (insert text)))

(defun lyrics-fetcher--open-lyrics (filename song-name)
  "Open lyrics for in FILENAME in ’lyrics-fetcher-lyrics-folder’.

SONG-NAME is the name of created buffer."
  (let ((buffer (generate-new-buffer song-name)))
    (with-current-buffer buffer
      (insert-file-contents (lyrics-fetcher--process-filename filename))
      (lyrics-fetcher-view-mode)
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
  "Keymap for ’lyrics-fetcher-mode’.")

(define-derived-mode lyrics-fetcher-view-mode read-only-mode "Lyrics view"
  "Major mode for viewing lyrics.

\\{lyrics-fetcher-view-mode-map}")
