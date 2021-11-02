# Versuri, the lyrics package for Emacs

- Supported websites: _makeitpersonal_, _genius_, _songlyrics_, _metrolyrics_,
_musixmatch_, _azlyrics_.

- Backed up with a local database.

- Search through the database interactively.

- Display lyrics in a text buffer or save them for later use.

- Synchronous bulk request for lyrics for a given list of songs.

- User extendable. Add other websites or modify the existing websites via the
`versuri-add-website` interface.

# Usage scenarios and examples

## Fetch and display lyrics from known websites

Display the lyrics in a read-only buffer. The lyrics are also saved in the database.
```emacs-lisp
(versuri-display "queen" "don't stop me now")
```

As above, but the lyrics are only saved in the database, not displayed.
```emacs-lisp
(versuri-save "queen" "don't stop me now")
```

A more generic interface:
- Pass a callback and do whatever you like with the received lyrics.
- Select a single, preferred lyrics website on which to search for lyrics, or a sub-list
of the available websites.
```emacs-lisp
(versuri-lyrics "queen" "don't stop me now"
                (lambda (lyrics)
                    (print lyrics))
                (list (versuri-find-website "musixmatch")
                      (versuri-find-website "genius")))
```

Save multiple lyrics at once for later use, say, for the top 10 songs from Queen.
Beware! This will block Emacs.
```emacs-lisp
(versuri-save-bulk
 (lastfm-artist-get-top-tracks "queen" :limit 10)
 10)
```

## Search artist and song from known lyrics

Search for all lyrics in the database that match a given string and
interactively select one of the entries with `completing-read`. With a
space before the string, select from all artists that matches the
string. With an empty string, select from all the songs in the
database.

For example, this will search for all songs that contain "tonight" in
their lyrics. Further filtering available thanks to a completion UI
like ivy, icomplete, etc.

```emacs-lisp
(versuri-search "tonight")
```
![image](https://user-images.githubusercontent.com/8273519/73678593-595b2780-46c1-11ea-9370-c53a0bb1158c.png)

`versuri-search` is interactive, so the search string can be given in
the minibuffer as well.

![image](https://user-images.githubusercontent.com/8273519/73678604-5f510880-46c1-11ea-95b0-df43d1f4fb66.png)

And yes, do whatever you want with the selected entry,
```emacs-lisp
(let* ((res (versuri-ivy-search "tonight"))
       (artist (car res))
       (song   (cadr res)))
  (format "%s - %s" artist song))
```

# A word of caution

Web-scraping large amounts of data is prone to get you banned from the
respective websites, so use this package in moderation. To try and prevent that,
all the lyrics are saved in a database. A second call to get the same lyrics,
will get the saved ones so no web-scraping takes place.

Additionally, each call is made to a random website, from the list of supported
websites. If the lyrics are not found on that random website, a new random
website is tried from the remaining ones, and so on and so forth. For the bulk
request, you can specify a timeout between two successive calls. Use a low value
if you're feeling adventurous and a high value if you have plenty of time and
especially if you have a large list of songs.

# Contributing

PRs for new websites or fixes for the supported ones are accepted. Ideas for
additional features are also accepted, though it doesn't necessarily mean they
will be implemented.

# Complete API

**versuri-mode**

    Major mode for versuri lyrics buffers.

**versuri-display** (artist song)

    Search and display the lyrics for ARTIST and SONG in a buffer.

    Async call.  When found, the lyrics are inserted in a new
    read-only buffer.  If the buffer with the same lyrics already
    exists, switch to it and don't create a new buffer.  Inside the
    buffer, the following keybindings are active:

    q: kill the buffer

    x: delete the entry from the database and kill the
    buffer.  Useful if you don't want to keep the lyrics around.

    r: find the lyrics on another website and redisplay the buffer.
    This is similar to 'x', but the lyrics is then searched and
    displayed again in a new buffer.  Not all websites have the same
    lyrics for the same song.  Some might be incomplete, some might
    be ugly.

**versuri-save** (artist song)

    Search and save the lyrics for ARTIST and SONG.

    Async call.  When found, the lyrics are saved in the database.
    If lyrics already in the database, do nothing.

**versuri-delete-lyrics** (artist song)

    Remove entry for ARTIST and SONG form the database.

**versuri-search** (str)

    Search the database for all entries that match STR.
    Use completing-read to let the user select one of the entries
    and return it.  Each entry contains the artist name, song name
    and a verse line.

    If STR is empty, this is a search through all the entries in the
    database.

    If STR starts with an empty space, this is a search for all
    artists that contain STR in their name.

    Otherwise, this is a search for all the lyrics that contain STR.
    There can be more entries with the same artist and song name if
    the STR matches multiple lines in the lyrics.

**versuri-add-website** (name template separator query)

    Define a new website where lyrics can be searched.
    If a website with the given NAME already exists, replace it.  If
    not, use the NAME, TEMPLATE SEPARATOR and QUERY to define a new
    lyrics website structure and add it to the list of known websites
    for lyrics searches.

    NAME is a user-friendly name of the website.

    TEMPLATE is the website url with placeholders for ${artist} and
    ${song}.  Replacing these templates with actual artist and song
    names results in a valid url that can be used to return the
    lyrics.

    SEPARATOR is used in conjunction with TEMPLATE to build the
    requested url.  The empty spaces in the artist and song name are
    replaced with SEPARATORs.  Some websites use dashes, others plus
    signs, for example.

    QUERY is used in the parsing phase of the html response.  It
    specifies the css selectors used by elquery to extract the lyrics
    part of the html page.

    See the already defined websites for examples for all of the
    above parameters.

**versuri-find-website** (name)

    Find a website by NAME in the list of defined websites.

**versuri-lyrics** (artist song callback &optional (websites versuri--websites))

    Pass the lyrics for ARTIST and SONG to the CALLBACK function.

    Async call. If the lyrics is found in the database, use that.
    Otherwise, search through WEBSITES for them. If found, save
    them to the database and recursivelly call this function again.

    By default, WEBSITES is bound to the list of all the known
    websites. To avoid getting banned, a random website is taken on
    every request. If the lyrics is not found on that website, repeat
    the call with the remaining websites.
      
**versuri-save-bulk** (songs max-timeout)

    Save the lyrics for all SONGS.

    SONGS is a list of '(artist song) lists.
    To avoid getting banned by the lyrics websites, wait a maximum of
    MAX-TIMEOUT seconds between requests.

    Sync call! Depending on the number of entries in the SONGS list,
    it can take a while.  In the meantime, Emacs will be blocked.
    Better use it while on a coffee break.
