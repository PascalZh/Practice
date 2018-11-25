#!/usr/bin/env racket
#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/dispatch
         web-server/page
         web-server/templates
         web-server/http/id-cookie)
(require net/url)
(require net/url-connect)
;(require db)
(require xml)
(require "../libcommon.rkt")
(require "manipulate_db.rkt")
(require "models.rkt")
(require "features/Go.rkt")

(struct UserInfo (username admin))

; 引擎同时只支持开一个，所以围棋同时只支持一个人在线
(def eng #f)
(def current-player #f)

(def (set-engine! eng_) 
  (set! eng eng_))

(define-values (dispatch url)
  (dispatch-rules
    [("") render/response-index]

    [("db" (string-arg) ...) (λ (req path) (redirect-to "/conf/not-found.html" permanently))]

    [("esp32") render/response-esp32]
    ; get_ip render the same page with esp32
    [("esp32" "get_ip") render/response-get-ip]

    [("login") render/response-login]

    [("test" (string-arg) ...) render/response-test]

    [("Go" (string-arg) ...) render/response-Go]
    ))

(def (start request)
  (initial-db)
  (dispatch request))


(def ip "还没有获取IP")
(def (render/response-esp32 request)
  (render/response-with-template 
    (include-template "./template/page_esp32.html")
    request))

(def (render/response-index request)
  (render/response-with-template
    (include-template "./template/page_index.html")
    request
    #:carousel? #t))

; get-ip {{{

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
    (render/response-esp32 request)
    (close-output-port s_out) (close-output-port s_err)))
; }}}

; login  {{{

(def (render/response-login request)
  ;(sh "../maze_foo.rkt")
  (def bindings (request-bindings request))
  (def username #f)
  (def userpwd #f)
  (def content #f)
  (when (and (exists-binding? 'username bindings)
             (exists-binding? 'userpwd bindings))
    (set! username (extract-binding/single
                     'username
                     bindings))
    (set! userpwd (extract-binding/single
                    'userpwd
                    bindings)))
  (if (and username userpwd)
    (let ([admin (query-account username userpwd)])
      (if (not admin)
        (begin
          (set! content "账号不存在或者密码错误！")
          (render/response-with-template 
            (include-template "./template/page_login.html")
            request))
        (begin
          ; 登录成功，跳转回主页
          ; 这里花了一晚上的时间调试都没有结果，提交表单时怎么也无法跳转到
          ; 这段代码，结果第二天继续DEBUG时发现，原来程序并没有出错！！！
          ; 是Firefox擅自把Cached的数据作为跳转页面，直接跳回主页了！！！
          ; 清除Firefox缓存就好了%>_<%
          ;(display "COOKIE HEADER: ")
          ;(response/xexpr "HELLO")
          ;(println (cookie->header
                     ;(make-login-cookie username userpwd)))
          (redirect-to "/" permanently
                       #:headers
                       (list
                         (cookie->header
                           (make-login-cookie username userpwd)))))))
    (begin
      (render/response-with-template 
        (include-template "./template/page_login.html")
        request))))
; }}}

; template {{{

(def (render/response-with-template
       content request
       #:carousel? [carousel? #f])

  (def login-content "")
  (let* ([client-cookies (request-cookies request)]
         [login-cookie (findf (λ (c)
                                ;(println (client-cookie-name c))
                                ;(println (client-cookie-value c))
                                (query-account (client-cookie-name c)
                                               (client-cookie-value c)))
                              client-cookies)])
    ; 如果是管理员
    (if (and login-cookie
               (= 1 (query-account
                      (client-cookie-name login-cookie)
                      (client-cookie-value login-cookie))))
      (set! login-content (render-admin-login-content))
      ; 不是管理员的话永远都看不到
      (set! carousel? #f)))

  ; 由于render/response-esp32和render/response-index ... 都要调用这个函数
  ; 而且搜索功能应该在两个页面中都能使用，所以在这里设置搜索页面
  (def html-content content)
  (when (exists-binding? 'search (request-bindings request))
    (set! html-content (render-search-page request)))

  (let ([navbar (include-template "./template/navbar_1.html")]
        [carousel (if carousel?
                    (include-template "./template/carousel_1.html")
                    "")])
    (page (response/xexpr #:cookies empty
            `(html ,(make-cdata
                      #f #f
                      (include-template "./template/template.html")))))))
; }}}

; test {{{

(def (render/response-test request path)
  (cond 
    [(= (length path) 0)
     (response/xexpr
       (let ([content (include-template "./template/page_test.html")])
         (render/response-with-template content request)))]

    [(and (string=? (car path) "add")
          (andmap string->number (cdr path)))
     (response/xexpr
       (number->string
         (apply
           (λ (l r)
             (+ (string->number l)
                (string->number r)))
           (cdr path))))]

    [(and (= (length path) 1)
          (string=? (car path) "test_ajax"))
     (response/xexpr
       (string->xexpr
         "<font size=\"4\" face=\"arial\" color=\"cyan\">A response from server</font>"))]

    [(and (= (length path) 1)
          (string=? (car path) "screen_shot"))
     (let*
       ([cur-time (current-seconds)]
        [err (call-with-output-string
               (λ (p)
                 (parameterize ([current-error-port p])
                   (system "scrot tmp/scrot.png")
                   (call-with-input-file
                     "./tmp/scrot.png" #:mode 'binary
                     (λ (in)
                       (screenshot-insert! cur-time (read-bytes 99999999 in)))))))])
       (if (= 0 (string-length err))
         (begin
           (response/xexpr
             (string-append
               "success! current time:"
               (number->string cur-time))))
         (response/xexpr
           err)))]

    [(and (= (length path) 1)
          (string=? (car path) "screen_shot_get_list"))
     ;(sh "../maze_foo.rkt")
     (response/xexpr
       (substring (apply string-append
                         (map
                           (λ (num)
                             (string-append
                               "|"
                               (number->string num)))
                           (query-screenshot-list)))
                  1))]

    [(and (= (length path) 2)
          (string=? (car path) "screen_shot_get")
          (number? (string->number (cadr path))))
     (let ([data (query-screenshot (string->number (cadr path)))])
       (when (eq? data #f)
         (set! data (call-with-input-file
                      "./images/404.jpg" #:mode 'binary
                      (λ (in)
                        (read-bytes 99999999 in)))))
       (response/full
         200 #"Get Screenshot"
         (current-seconds) TEXT/HTML-MIME-TYPE
         (list (make-header #"Content-Type"
                            #"image/png"))
         (list data)))]

    [else
      (response/xexpr
        (with-output-to-string
          (λ ()
            (print
              ;path
              (request-uri request)
              ))))]))
; }}}

; Go {{{
(def (render/response-Go request path)
  (cond
    ; start 负责开启engine
    [(string=? (car path) "start")
     (let
       ([player_name (cadr path)]
        [engine_name (string->symbol (caddr path))])
       (if (not eng)
         (set-engine! (init-engine engine_name))
         (when (not (eq? engine_name (engine-name eng)))
           ((engine-ctrl eng) 'kill)
           (set-engine! (init-engine engine_name))))
       (if (not current-player)
         (begin
           (set! current-player player_name)
           (response/xexpr "start ok"))
         (response/xexpr current-player)))]

    [(string=? (car path) "play")
     (if (not eng)
       (response/xexpr "no engine")
       (let ([n (next-move (cadr path) eng)])
         (response/xexpr n)))]

    [(string=? (car path) "clear_board")
     (write-gtp "clear_board" eng)
     (let ([ret (read-gtp eng)])
       (if (string=? "= " ret)
         (response/xexpr "clear_board ok")
         (response/xexpr ret)))]

    ; stop 负责关闭engine
    [(string=? (car path) "stop")
     (set! current-player #f)
     ((engine-ctrl eng) 'kill)
     (set! eng #f)
     (response/xexpr "stop ok")]
    ))
; }}}


(def (make-login-cookie name pwd)
  (make-cookie
    name pwd
    #:expires (seconds->date (+ (current-seconds) (* 60 60 24 15)))))
; }}} end implementation

(def (urlopen url)
  (define-values
    (status headers port)
    (http-sendrecv/url (string->url url)
                       #:method "GET"))
  (begin0
    (read port)
    (close-input-port port)))

(module* main #f

  ;(current-error-port (open-output-file "log/error.log" #:exists 'append))
  ;(current-output-port (open-output-file "log/output.log" #:exists 'append))
  (def request-output (open-output-file "log/request.log" #:exists 'append))

  (unless (directory-exists? "tmp")
    (make-directory "tmp"))

  (thread (λ () (system "features/filter_request_log.py")))

  (serve/servlet start
                 #:port 8888
                 #:stateless? #t
                 #:servlet-path "/"
                 #:servlet-regexp #rx""
                 #:listen-ip #f
                 #:extra-files-paths
                 (list (build-path "."))
                 #:server-root-path "."
                 #:servlets-root "."
                 #:command-line? #t
                 #:log-file request-output
                 ;#:ssl? #t
                 ;#:ssl-cert "./server-cert.pem"
                 ;#:ssl-key "./private-key.pem"
                 ))
