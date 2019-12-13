(require 'cl-lib)
(require 'request)
(require 'mpv)
(require 'ivy-youtube)
(require 's)
(require 'esqlite)

(defconst db (esqlite-stream-open "~/Downloads/cl-lyrics.db"))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun lyrics-db-read (query)
  (esqlite-stream-read db query))

(defun search-song (query-str)
  "Return a list of (\"artist-name | song-name | lyrics-line\"),
where lyrics-line is the line that contains the `query-str'."
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
                                        full-line)))))
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
                      (let ((song-no-sep
                             (split-string (cl-first song) "|")))
                        (listen-on-youtube
                         (concat (cl-first song-no-sep)
                                 (cl-second song-no-sep)))))))

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

(listen-on-youtube "void of silence dark static moments")

(lyrics-lyrics "you")

(mpv-pause)

