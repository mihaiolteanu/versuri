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

(defun get-lyrics (artist song)
  (lyrics-db-read
   (format "SELECT lyrics FROM lyrics WHERE artist=\"%s\" AND song=\"%s\""
           artist song)))

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
  "Select and return an entry from the lyrics db."
  (interactive "MSearch lyrics: ")
  (let (res)
    (ivy-read "Select Lyrics: "
            (search-song query-str)
            :action (lambda (song)
                      (print song)
                      (setf res (concat (cl-second song) " "
                                        (cl-third song)))))
    res))

;;;; Get the lyrics with elquery
;; elquery-read-string removes all newlines (issue on github created but no
;; response), so I've defined a new one, but this uses an internal elquery defun

(defun my-elquery-read-string (string)
  "Like the original elquery-read-string, but don't remove spaces."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert string)
    (let ((tree (libxml-parse-html-region (point-min) (point-max))))
      (thread-last tree
        (elquery--parse-libxml-tree nil)))))

(defconst lyrics-websites nil)

(cl-defstruct lyrics-website
  name template separator query)

(defun add-lyrics-website (name template separator query)
  (let ((new-website (make-lyrics-website
                      :name name
                      :template template         
                      :separator separator
                      :query query)))
    ;; Replace the entry if there is already a website with the same name.
    (aif (cl-position name lyrics-websites
                      :test (lambda (a b) (equal a b))
                      :key #'lyrics-website-name)
        (setf (nth it lyrics-websites) new-website)
      ;; Freshly add it, otherwise.
      (push new-website lyrics-websites))))

(defun lyrics-website (name)
  "Find a lyrics website from the list of known websites."
  (cl-find-if (lambda (w)
                (equal w name))
              lyrics-websites
              :key #'lyrics-website-name))

(defun random-lyrics-website ()
  (nth (random (length lyrics-websites))
       lyrics-websites))

(add-lyrics-website "makeitpersonal"
  "https://makeitpersonal.co/lyrics?artist=${artist}&title=${song}"
  "-" nil)

(add-lyrics-website "genius"
  "https://genius.com/${artist}-${song}-lyrics"
  "-" "div.lyrics p")

(add-lyrics-website "songlyrics"
  "http://www.songlyrics.com/${artist}/${song}-lyrics/"
  "-" "p#songLyricsDiv")

(add-lyrics-website "metrolyrics"
  "http://www.metrolyrics.com/~${song}-lyrics-~${artist}.html"
  "-" "p.verse")

(add-lyrics-website "musixmatch"
  "https://www.musixmatch.com/lyrics/${artist}/${song}"
  "-" "p.mxm-lyrics__content")

(add-lyrics-website "azlyrics"
  "https://www.azlyrics.com/lyrics/${artist}/${song}.html"
  "-" "div.container.main-page div.row div:nth-child(2) div:nth-of-type(5)")

(defun build-url (website artist song)
  (let ((sep (lyrics-website-separator website)))
    (s-format (lyrics-website-template website)
              'aget
              `(("artist" . ,(s-replace " " sep artist))
                ("song"   . ,(s-replace " " sep song))))))

(defun request-lyrics (artist song &optional website)
  (or (get-lyrics artist song)          ;from db, if it exists.
      ;; Either use the specified website, if it exists, or pick a random one.
      (let ((website (if website
                         (aif (lyrics-website website)
                             it
                           (error "No such website in the database."))
                       (random-lyrics-website)))
            (response))
        (request (build-url website artist song)
                 :parser 'buffer-string
                 :sync t
                 :success (cl-function
                           (lambda (&key data &allow-other-keys)
                             (setf response data))))        
        (when response
          (elquery-text
           (cl-first (elquery-$ (lyrics-website-query website)
                                (my-elquery-read-string response))))))))
