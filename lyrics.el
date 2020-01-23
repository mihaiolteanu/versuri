;;; lyrics.el --- Playing with lyrics -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'request)
(require 'elquery)
(require 'mpv)
(require 'ivy-youtube)
(require 's)
(require 'esqlite)

(defconst lyrics-db (esqlite-stream-open "~/Downloads/cl-lyrics.db"))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun lyrics-db-read (query)
  (esqlite-stream-read lyrics-db query))

(defun db-get-lyrics (artist song)
  (aif (lyrics-db-read
        (format "SELECT lyrics FROM lyrics WHERE artist=\"%s\" AND song=\"%s\""
                artist song))
      (cl-first (cl-first it))))

(defun db-get-lyrics-like (str)
  (lyrics-db-read
   (format "SELECT * from lyrics WHERE lyrics like '%% %s %%'" str)))

(defun db-save-lyrics (artist song lyrics)
  (esqlite-stream-execute lyrics-db
   (format "INSERT INTO lyrics(artist,song,lyrics) VALUES(\"%s\", \"%s\", \"%s\")"
           artist song lyrics)))

(defun search-song (str)
  "Query the database for all the lyrics lines that match the
`str'. For each match, return that verse line together with
the artist and song name."
  (cl-remove-duplicates
   (mappend (lambda (song)
              ;; Only keep the matches. Unmached regexes will return nil.
              (cl-remove-if #'null                                        
                            (mapcar (lambda (line)
                                      (let ((full-line
                                             ;; Get the whole line that matches the str
                                             (cl-first (s-match (concat ".*"
                                                                        str ".*")
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
            (db-get-lyrics-like str))
   ;; No need to keep identical entries
   :test (lambda (a b)
           (string= (cl-first a) (cl-first b)))))

(defun lyrics-lyrics (query-str)
  "Select and return an entry from the lyrics db."
  (interactive "MSearch lyrics: ")
  (let (res)
    (ivy-read "Select Lyrics: "
            (search-song query-str)
            :action (lambda (song)
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

(defun request-lyrics (website artist song callback)
  (let (resp)
    (request (build-url website artist song)
           :parser 'buffer-string
           :sync nil
           :success (cl-function
                     (lambda (&key data &allow-other-keys)
                       (funcall callback data)))))
  nil)

(defun parse-lyrics (website html)
  (let* ((css (lyrics-website-query website))
         (parsed (elquery-$ css (my-elquery-read-string html)))
         (lyrics (elquery-text (cl-first parsed))))
    lyrics))

(defun display-lyrics (artist song)
  (if-let ((lyrics (db-get-lyrics artist song)))
      (let ((b (generate-new-buffer
                (format "%s - %s | lyrics" artist song))))
        (with-current-buffer b
          (insert (format "%s - %s\n\n" artist song))
          (insert lyrics))
        (switch-to-buffer b))
    (let ((website (lyrics-website "genius")))
      (request-lyrics
       website artist song
       (lambda (resp)
         (let ((lyrics (parse-lyrics website html)))           
           (db-save-lyrics artist song lyrics)
           (display-lyrics artist song)))))))

