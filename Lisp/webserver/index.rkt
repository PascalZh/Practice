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
(require "model_search.rkt")

(struct UserInfo (username admin))
(define-values (dispatch url)
  (dispatch-rules
    [("") render/response-index]

    [("esp32") render/response-esp32]
    ; get_ip render the same page with esp32
    [("get_ip") render/response-get-ip]

    [("login") render/response-login]
    ))

(def (start request)
  (initial-db)
  (dispatch request))


(def ip "还没有获取IP")
; implementation: render {{{
(def (render/response-esp32 request)
  (render/response-with-template 
    (include-template "./template/page_esp32.html")
    request))

(def (render/response-index request)
  (render/response-with-template
    (include-template "./template/page_index.html")
    request
    #:carousel? #t))

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
         [search-html (render-search-items
                        search-name
                        (query-fortune))])
    (include-template "./template/page_search.html")))

(def (render/response-login request)
  (def bindings (request-bindings request))
  (def username "")
  (def userpwd "")
  (def content "")
  (if (and (exists-binding? 'username bindings)
           (exists-binding? 'userpwd bindings))
    (begin
      (set! username (extract-binding/single
                       'username
                       bindings))
      (set! userpwd (extract-binding/single
                      'userpwd
                      bindings))
      (let ([admin (query-account username userpwd)])
        (if (eq? admin #f)
          (begin
            (set! content "账号不存在或者密码错误！")
            (render/response-with-template 
              (include-template "./template/page_login.html")
              request))
          ; 登录成功，跳转回主页
          ;(render/response-index request)
          (redirect-to "/" permanently)
          )))
    (render/response-with-template 
      (include-template "./template/page_login.html")
      request)))


(def (render/response-with-template
       content request
       #:carousel? [carousel? #f])

  ; 由于render/response-esp32和render/response-index ... 都要调用这个函数
  ; 而且搜索功能应该在两个页面中都能使用，所以在这里设置搜索页面
  (def html-content content)
  (when (exists-binding? 'search (request-bindings request))
    (set! html-content (render-search-page request)))

  (let ([navbar (include-template "./template/navbar_1.html")]
        [carousel (if carousel? (include-template "./template/carousel_1.html")
                    "")])
    (page
      (response/xexpr
        `(html ,(make-cdata #f #f (include-template "./template/template.html")))))))
; }}} end implementation

(def (urlopen url)
  (define-values
    (status headers port)
    (http-sendrecv/url (string->url url)
                       #:method "GET"))
  (read port))


(current-error-port (open-output-file "log/error.log" #:exists 'append))
(current-output-port (open-output-file "log/output.log" #:exists 'append))
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
