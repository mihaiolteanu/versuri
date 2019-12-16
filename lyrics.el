;;; lyrics.el --- Playing with lyrics -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'request)
(require 'elquery)
(require 'mpv)
(require 'ivy-youtube)
(require 's)
(require 'esqlite)

(defconst db (esqlite-stream-open "~/Downloads/cl-lyrics.db"))
(defconst *socket* "/tmp/mpv-cl-socket")

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun lyrics-db-read (query)
  (esqlite-stream-read db query))

(defun search-song (query-str)
  "Query the database for all the lyrics lines that match the
`query-str'. For each match, return that verse line together with
the artist and song name."
  (let* ((raw-songs (lyrics-db-read
                     (concat "SELECT * from lyrics WHERE lyrics like '%"
                             query-str "%'"))))
    ;; raw-songs, as received from the db, contains all the info there is for
    ;; one entry in the lyrics db. That means, it contains the whole
    ;; lyrics. We're interested in just the line where the match occurs. 
    (cl-remove-duplicates
     (mappend (lambda (song)
                ;; Only keep the matches. Unmached regexes will return nil.
                (cl-remove-if #'null                                        
                 (mapcar (lambda (line)
                      (let ((full-line
                             ;; Get the whole line that matches the query-str
                             (cl-first (s-match (concat ".*"
                                                        query-str ".*")
                                                        line))))
                        (unless (null full-line)
                          ;; Add the artist and song name to the matched line.
                          (list (concat (cl-second song) " | "
                                        (cl-third song)  " | "
                                        full-line)
                                (cl-second song)
                                (cl-third song)))))
                    ;; For each lyrics line for the current entry in the db.
                    (s-lines (cl-fourth song)))))
              ;; For each entry in the db.
              raw-songs)
     ;; No need to keep identical entries
     :test (lambda (a b)
             (string= (cl-first a) (cl-first b))))))

(defun lyrics-lyrics (query-str)
  (interactive "MSearch lyrics: ")
  (ivy-read "Select Lyrics: "
            (search-song query-str)
            :action (lambda (song)
                      (listen-on-youtube
                       (concat (cl-second song) " "
                               (cl-third song))))))

(mpv-pause)
(lyrics-lyrics "")

(defun youtube-response-id (*qqJson*)
  "Copied from ivy-youtube-wrapper in mpv.el"
  (let ((search-results (cdr (ivy-youtube-tree-assoc 'items *qqJson*))))
    (cdr (ivy-youtube-tree-assoc 'videoId (aref search-results 0)))))

(defun browse-youtube (video-id)
  (browse-url (concat "https://www.youtube.com/watch?v="
                      video-id)))

(defun play-youtube-video (video-id)
  (mpv-start "--no-video"
             (concat "https://www.youtube.com/watch?v="
                    video-id)))

(defun listen-on-youtube (query-str)
  (request
   "https://www.googleapis.com/youtube/v3/search"
   :params `(("part" . "snippet")
	     ("q" . ,query-str)
	     ("type" . "video")
	     ("maxResults" . 1)
	     ("key" .  ,ivy-youtube-key))
   :parser 'json-read
   :success (cl-function
	     (lambda (&key data &allow-other-keys)
               (play-youtube-video (youtube-response-id data))))
   :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
		  ;; (200 . (lambda (&rest _) (message "Got 200.")))
		  (418 . (lambda (&rest _) (message "Got 418.")))
                  (403 . (lambda (&rest _)
                           (message "403: Unauthorized. Maybe you need to enable your youtube api key"))))
   :complete (message "searching...")))


(defun my-listen (my-list)
  (unless (null my-list)
    (async-start (lambda ()
                   (sleep-for 1)
                   (car my-list))
                 (lambda (result)
                   (message "---> %s" result)
                   (my-listen (cdr my-list))))))
;; (my-listen '(1 2 3))

;; (mpv--enqueue '("get_property" "duration") (lambda (x) (print x)))
;; (mpv--enqueue '("get_property" "time-pos") (lambda (x) (print x)))

;; (listen-on-youtube "void of silence dark static moments")
;; (mpv-pause)




;;;; Get the lyrics with elquery
;; elquery-read-string removes all newlines (issue on github created but no
;; response), so I've defined a new one, but this uses an internal elquery defun

(defconst lyrics-raw-websites
  '((makeitpersonal
     "https://makeitpersonal.co/lyrics?artist=artist-name&title=song-name"
     ?- nil)
    (genius
     "https://genius.com/artist-name-song-name-lyrics"
     ?- "div.lyrics p")
    (songlyrics
     "http://www.songlyrics.com/artist-name/song-name-lyrics/"
     ?- /n)
    (metrolyrics
     "http://www.metrolyrics.com/~song-name-lyrics-~artist-name.html"
     ?- "p.verse")
    (musixmatch
     "https://www.musixmatch.com/lyrics/artist-name/song-name"
     ?- "p.mxm-lyrics__content")
    (azlyrics
     "https://www.azlyrics.com/lyrics/artist-name/song-name.html"
     nil "div.container.main-page div.row div:nth-child(2) div:nth-of-type(5)")))

(defconst genius-resp nil)
(defconst songlyrics-resp nil)

(defun my-elquery-read-string (string)
  "Return the AST of the HTML string STRING as a plist."
  (with-temp-buffer
    (set-buffer-multibyte nil) ; ref debbugs.gnu.org/cgi/bugreport.cgi?bug=31427
    (insert string)
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (thread-last tree
        (elquery--parse-libxml-tree nil)))))

(request "https://genius.com/anathema-thin-air-lyrics"
         ;;"http://www.songlyrics.com/anathema/thin-air-lyrics/"
         :parser 'buffer-string
         :success (cl-function
                   (lambda (&key data &allow-other-keys)
                     (setq genius-resp data))))

(elquery-text
 (cl-first (elquery-$ "p#songLyricsDiv"
                      (elquery-read-string songlyrics-resp t))))

(elquery-text
 (cl-first (elquery-$ "div.lyrics p"
                      (my-elquery-read-string genius-resp))))

(with-temp-buffer
  (insert genius-resp)
  (libxml-parse-html-region (buffer-end -1) (buffer-end 1)))


