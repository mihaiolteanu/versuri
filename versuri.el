;;; lyrics.el --- Playing with lyrics -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'dash)
(require 'request)
(require 'elquery)
(require 's)
(require 'esqlite)

(defconst versuri--db-stream
  (let ((db (concat (xdg-config-home) "/versurii.db")))    
    (esqlite-execute db   
     (concat "CREATE TABLE IF NOT EXISTS lyrics ("
             "id     INTEGER PRIMARY KEY AUTOINCREMENT "
             "               UNIQUE "
             "               NOT NULL, "
             "artist TEXT    NOT NULL "
             "               COLLATE NOCASE, "
             "song   TEXT    NOT NULL "
             "               COLLATE NOCASE, "
             "lyrics TEXT    COLLATE NOCASE);"))
    (esqlite-stream-open db)))

(defun mappend (fn list)
  (apply #'append (mapcar fn list)))

(defun lyrics-db-read (query)
  (esqlite-stream-read versuri--db-stream query))

(defun db-get-lyrics (artist song)
  (aif (lyrics-db-read
        (format "SELECT lyrics FROM lyrics WHERE artist=\"%s\" AND song=\"%s\""
                artist song))
      (cl-first (cl-first it))))

(defun db-get-lyrics-like (str)
  (lyrics-db-read
   (format "SELECT * from lyrics WHERE lyrics like '%% %s %%'" str)))

(defun db-save-lyrics (artist song lyrics)
  (esqlite-stream-execute versuri--db-stream
   (format "INSERT INTO lyrics(artist,song,lyrics) VALUES(\"%s\", \"%s\", \"%s\")"
           artist song lyrics)))

(defun build-ivy-entryline (verse song str)
  ;; Get the whole line that matches the str
  (if-let ((full-line (cl-first
                       (s-match (format ".*%s.*" str) verse))))
      (list (format "%s | %s | %s" (cl-second song)
                    (cl-third song)
                    full-line)
            (cl-second song)
            (cl-third song))))

(defun search-song (str)
  "Query the database for all the lyrics lines that match the
`str'. For each match, return that verse line together with
the artist and song name."
   (seq-uniq
    (seq-remove #'null
     (mappend (lambda (song)
                (mapcar (lambda (verse)
                     (build-ivy-entryline verse song str))
                   (s-lines (cl-fourth song))))
              (db-get-lyrics-like str)))))

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
  "-" "p")

(add-lyrics-website "genius"
  "https://genius.com/${artist}-${song}-lyrics"
  "-" "div.lyrics p")

(add-lyrics-website "songlyrics"
  "http://www.songlyrics.com/${artist}/${song}-lyrics/"
  "-" "p#songLyricsDiv")

(add-lyrics-website "metrolyrics"
  "http://www.metrolyrics.com/${song}-lyrics-${artist}.html"
  "-" "p.verse")

(add-lyrics-website "musixmatch"
  "https://www.musixmatch.com/lyrics/${artist}/${song}"
  "-" "p.mxm-lyrics__content span")

(add-lyrics-website "azlyrics"
  "https://www.azlyrics.com/lyrics/${artist}/${song}.html"
  "" "div.container.main-page div.row div:nth-child(2) div:nth-of-type(5)")

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
                       (funcall callback data)))
           :error (cl-function
                   (lambda (&key data &allow-other-keys)
                     (funcall callback nil)))))
  nil)

(defun parse-lyrics (website html)
  (let* ((css (lyrics-website-query website))
         (parsed (elquery-$ css (my-elquery-read-string html)))
         (lyrics))
    ;; Some lyrics are split into multiple elements (musixmatch), otherwise, an
    ;; (elquery-text (car el)) would have been enough.
    (mapcar (lambda (el)
         (let ((text (elquery-text el)))
           (setf lyrics (concat                         
                         (if (equal (lyrics-website-name website) "songlyrics")
                             (s-replace "" "" text)
                           text)
                         "\n\n"
                         lyrics))))
       parsed)
    lyrics))

(cl-defun display-lyrics (artist song &optional (websites lyrics-websites))
  "By default, the function starts with all the known
websites. To avoid getting banned, I take a random website on
every request. If the lyrics is not found on that website, remove
it from the list and try again with another random website from
the remaining list."
  (when websites
    (if-let ((lyrics (db-get-lyrics artist song)))
        (let ((b (generate-new-buffer
                  (format "%s - %s | lyrics" artist song))))
          (with-current-buffer b
            (insert (format "%s - %s\n\n" artist song))
            (insert lyrics))
          (switch-to-buffer b))
      (let ((website (random-lyrics-website)))
        (request-lyrics website artist song
         (lambda (resp)
           (if (and resp
                    ;; makeitpersonal
                    (not (s-contains? "Sorry, We don't have lyrics" resp)))
               (let ((lyrics (parse-lyrics website resp)))
                 (when lyrics
                   (db-save-lyrics artist song lyrics)
                   (display-lyrics artist song)))
             ;; Lyrics not found, try another website.
             (display-lyrics artist song
                             (-remove-item website lyrics-websites)))))))))

