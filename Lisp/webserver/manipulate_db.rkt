#!/usr/bin/env racket
#lang racket
(require db)
(require file/md5)
(require "../libcommon.rkt")

(provide initial-db
         query-fortune
         query-account account-insert!
         admin-name admin-pwcred
         (contract-out
           [screenshot-insert! (-> number? bytes? any/c)]
           [query-screenshot-list (-> any/c)]
           [query-screenshot (-> natural-number/c (or/c bytes? boolean?))]))

(def metadata-1 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)")
(def metadata-2 "(id INTEGER PRIMARY KEY, username TEXT, userpwd TEXT, admin INTEGER)")
(def metadata-3 "(timestamp INTEGER, picture BLOB)")

(def db 0)
(def admin-name "pascal")
(def admin-pwcred (bytes->string/utf-8 (md5 "8f0ebf4cca28c7936fd6c15774bab9c5")))

(def (initial-db)
  (unless (directory-exists? "db")
    (make-directory "db"))
  (set! db (sqlite3-connect #:database "db/data.db" #:mode 'create))

  (unless (table-exists? db "fortune")
    (query-exec db
      (string-append
        "CREATE TABLE fortune"
        metadata-1))
    (fortune-insert! (string-append
                       "run fortune at "
                       (sh "date"))
                     (sh "fortune")))

  (unless (table-exists? db "accounts")
    (query-exec db
      (string-append
        "CREATE TABLE accounts"
        metadata-2))
    (account-insert! admin-name admin-pwcred 1))

  (unless (table-exists? db "screenshots")
    (query-exec db
      (string-append
        "CREATE TABLE screenshots"
        metadata-3)))
  )

; implementation {{{
(def (query-fortune)
  (fortune-insert! (string-append
                     "run fortune at "
                     (sh "date"))
                   (sh "fortune"))
  (query-rows db "SELECT * FROM fortune"))

(def (fortune-insert! title body)
  (query-exec
    db
    "INSERT INTO fortune (title, body) VALUES (?, ?)"
    title body))

(def (query-account acc pwd)
  (def pwcred (query-maybe-value
             db
             "SELECT userpwd FROM accounts WHERE username = ?"
    acc))
  (def input-pwcred (bytes->string/utf-8 (md5 pwd)))

  (if (eq? #f pwcred)
    pwcred
    (if (not (string=? pwcred input-pwcred))
      #f
      (query-value
        db
        "SELECT admin FROM accounts WHERE username = ?"
        acc))))

(def (account-insert! acc pwd admin)
  (query-exec
    db
    "INSERT INTO accounts (username, userpwd, admin) VALUES (?, ?, ?)"
    acc pwd admin))

(def (screenshot-insert! timestamp picture)
  (display "DEBUG:screenshot-insert!:(bytes-length picture) ")
  (displayln (bytes-length picture))
  (query-exec
    db
    "INSERT INTO screenshots (timestamp, picture) VALUES (?, ?)"
    timestamp picture))

(def (query-screenshot-list)
  (query-list
    db
    "SELECT timestamp FROM screenshots"))

(def (query-screenshot timestamp)
  (query-maybe-value
    db
    "SELECT picture FROM screenshots WHERE timestamp = ?"
    timestamp))

; }}} end implementation

