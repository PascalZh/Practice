#!/usr/bin/env racket
#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/page
         web-server/templates)
(require net/url)
(require net/url-connect)
;(require db)
(require xml)
(require "../libcommon.rkt")
(require "manipulate_db.rkt")

(define-values (dispatch url)
  (dispatch-rules
    [("") render/response-index]
    [("esp32") render/response-esp32]
    [("get_ip") render/response-get-ip]))

(def (start request)
  (initial-db)
  (dispatch request))

(def (render/response-esp32 request)
  (render/response-with-template 
    (include-template "./template/content_esp32.html")
    request))

(def (render/response-index request)
  (render/response-with-template
    (include-template "./template/content_index.html")
    request))

(def (render/response-get-ip request)
  (let*
    ([s_out (open-output-string)]
     [s_err (open-output-string)]
     [ip_
       (parameterize ([current-output-port s_out]
                      [current-error-port s_err])
         (system "../../MCU/ESP32/http_server/get_ip_by_serial.py")
         (let
           ([s_out_html (string-replace (get-output-string s_out) "\n" "<br/>")]
            [s_err_html (string-replace (get-output-string s_err) "\n" "<br/>")])
           (if (= 0 (string-length s_out_html))
             (string-append "<div class=\"alert alert-danger\">获取IP地址错误, 错误信息:</div>"
                            "<div class=\"pre-scrollable\">" s_err_html "</class>")
             s_out_html))
         )])
    (set! ip ip_)
    (render/response-esp32 request)))

; return html as a string
(def (render-search-page request)
  (let* ([search-name (extract-binding/single
                        'search
                        (request-bindings request))]
         [search-html (render-search-items search-name (query-search search-name))])
    (include-template "./template/content_search.html")))

(def ip "还没有获取IP")
(def html-content "")
(def (render/response-with-template content request)

  ; 由于render/response-esp32和render/response-index都要调用这个函数
  ; 而且搜索功能应该在两个页面中都能使用，所以在这里设置搜索页面
  (set! html-content content)
  (when (exists-binding? 'search (request-bindings request))
    (set! html-content (render-search-page request)))

  (let ([navbar (include-template "./template/navbar_1.html")])
    (page
      (response/xexpr
        `(html ,(make-cdata #f #f (include-template "./template/template.html")))))))

(def (urlopen url)
  (define-values
    (status headers port)
    (http-sendrecv/url (string->url url)
                       #:method "GET"))
  (read port))


(serve/servlet start
               #:port 8888
               #:servlet-path "/"
               #:servlet-regexp #rx""
               #:listen-ip #f
               #:extra-files-paths
               (list (build-path "."))
               #:server-root-path "."
               #:servlets-root "."
               #:command-line? #t
               ;#:ssl? #t
               ;#:ssl-cert "./server-cert.pem"
               ;#:ssl-key "./private-key.pem"
               )
