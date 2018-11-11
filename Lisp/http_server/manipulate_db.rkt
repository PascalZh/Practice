#!/usr/bin/env racket
#lang racket
(require db)
(require "../libcommon.rkt")

(provide initial-db render-search-items query-search)

(def str-metadata-1 "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)")
(def db 0)

(def (gayety-insert! title body)
  (query-exec
    db
    "INSERT INTO gayety (title, body) VALUES (?, ?)"
    title body))

(def (initial-db)
  (unless (directory-exists? "db")
    (make-directory "db"))
  (set! db (sqlite3-connect #:database "db/all.db" #:mode 'create))
  (unless (table-exists? db "gayety")
    (query-exec db
      (string-append
        "CREATE TABLE gayety"
        str-metadata-1))
    (gayety-insert! (string-append
                      "run fortune at "
                      (remove\n (sh "date")))
                    (remove\n (sh "fortune")))))

(def (query-search str)
  (gayety-insert! (string-append
                    "run fortune at "
                    (remove\n (sh "date")))
                  (remove\n (sh "fortune")))
  (query-rows db "SELECT * FROM gayety"))

(def (render-search-items search-name query-result)
  (def (render-gayety)
    (css-search
      (css-search-item
        "<div class=\"table-responsive\"><table class=\"table table-bordered table-hover\">"
        "<thead><tr><td>key</td><td>command</td><td>result</td></tr></thead>"
        "<tbody>"
        (apply string-append
               (map (λ (vect)
                      (string-append
                        "<tr>"
                        "<td>"
                        (number->string (vector-ref vect 0))
                        "</td>"
                        "<td>"
                        (vector-ref vect 1)
                        "</td>"
                        "<td>"
                        (vector-ref vect 2)
                        "</td>"
                        "</tr>"))
                    query-result))
        "</tbody>"
        "</table></div>")))
  (render-gayety))

(def (css-search-item str . rest)
  (string-append
    "<div class=\"search-item\">"
    str
    (apply string-append rest)
    "</div>"))
(def (css-search str . rest)
  (string-append
    "<div class=\"search\">"
    str
    (apply string-append rest)
    "</div>"))
