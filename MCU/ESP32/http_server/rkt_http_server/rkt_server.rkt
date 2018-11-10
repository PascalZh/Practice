#!/usr/bin/env racket
#lang racket
(require web-server/servlet
         web-server/servlet-env)
(require net/url)
(require net/url-connect)
(require )

(require "../../../Lisp/libcommon.rkt")

(define (rkt_server request)
  (response/xexpr
   '(html
     (head (title "李时珍的皮"))
     (body (h1 (font ((size "40") (color "#FF1020")) "顾凯峰的博客" (br)))
           (div "顾凯峰是著名的科学家，哲学家，政治学家，物理家，艺术家，这里是他的博客")
           (br)
           (div "他诗如李白，歌如刘德华，字如王羲之，智慧如诸葛亮，乒乓球如马云，是各个方面都达到人类巅峰的全能型人物。")
           (br)
           (img ((src "https://ss0.bdstatic.com/70cFvHSh_Q1YnxGkpoWK1HF6hhy/it/u=2821871173,3198763803&fm=26&gp=0.jpg")))
           (br)))))

;(def (urlopen url)
  ;(define-values
    ;(status headers port)
    ;(http-sendrecv/url (string->url "http://192.168.43.156:1234/hello")
                       ;#:method "GET"))
  ;(def data (read port))
  ;(println data))


(serve/servlet rkt_server
               #:port 8888
               #:servlet-path "/"
               #:listen-ip #f)
