;; -*- lexical-binding: t -*-

(require 'request)
(require 'json)
(require 'seq)
(require 'shr)

(defcustom lyrics-fetcher-genius-access-token nil
  "Genius access token. Get one at https://genius.com."
  :type '(string nil)
  :group 'lyrics-fetcher)

(defun lyrics-fetcher--genius-do-search (query callback &optional sync)
  (if (string-empty-p lyrics-fetcher-genius-access-token)
      (message "Genius client access token not set!")
    (message "Sending a query to genius API...")
    (request "https://api.genius.com/search"
      :params `(("q" . ,query) ("access_token" . ,lyrics-fetcher-genius-access-token))
      :parser 'json-read
      :sync sync
      :success callback)))

(defun lyrics-fetcher-genius--format-song-title (entry)
  (let ((result (assoc 'result entry)))
    (format "%-40s [lyrics: %s]"
            (cdr (assoc 'full_title result))
            (cdr (assoc 'lyrics_state result)))))

(defun lyrics-fetcher--genius-get-url-from-response (data)
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
         (if (= (prefix-numeric-value current-prefix-arg) 4)
             (let ((results-songs-for-select
                    (mapcar
                     (lambda (entry)
                       (cons (lyrics-fetcher-genius--format-song-title entry)
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
                              (point-max)))))))))

(lyrics-fetcher--genius-do-search
 "Epica Rivers"
 (cl-function
  (lambda (&key data &allow-other-keys)
    (setq my/test data))))

(defun my/foo ()
  (defun my/bar () 1)
  (my/bar))

(add-to-list 'ivy-prescient-sort-commands
             'lyrics-fetcher--genius-get-url-from-response t)

(let ((current-prefix-arg '(4)))
  (lyrics-fetcher--genius-get-url-from-response my/test))

(lyrics-fetcher--genius-fetch-lyrics
 "https://genius.com/Nightwish-the-greatest-show-on-earth-lyrics"
 (lambda (res)
   (setq my/res res)))
