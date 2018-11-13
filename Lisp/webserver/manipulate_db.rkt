#!/usr/bin/env racket
#lang racket
(require db)
(require file/md5)
(require "../libcommon.rkt")

(provide initial-db
         query-fortune
         query-account account-insert!)

(def metadata-1 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)")
(def metadata-2 "(id INTEGER PRIMARY KEY, username TEXT, userpwd TEXT, admin INTEGER)")
(def db 0)
(def admin-pwcred (bytes->string/utf-8 (md5 "8f0ebf4cca28c7936fd6c15774bab9c5")))

(def (initial-db)
  (unless (directory-exists? "db")
    (make-directory "db"))
  (set! db (sqlite3-connect #:database "db/all.db" #:mode 'create))

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
    (account-insert! "pascal" admin-pwcred 1)))

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
; }}} end implementation

