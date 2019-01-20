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
(def eng1 #f)
(def eng2 #f)
(def current-player #f)

(def (set-engine! eng_) 
  (set! eng1 eng_))

(define-values (dispatch url)
  (dispatch-rules
    [("") render/response-index]

    [("db" (string-arg) ...) (λ (req path) (redirect-to "/conf/not-found.html" permanently))]

    [("esp32") render/response-esp32]
    ; get_ip render the same page with esp32
    [("esp32" "get_ip") render/response-get-ip]

    [("login") render/response-login]

    [("test" (string-arg) ...) render/response-test]

    [("Go" (string-arg) ...) render/response-Go]))

; don't use define-syntax-rule, because include-template seams
; not to use the context where it expands.
; use datum->syntax to explicitly pass context to the syntax
(define-syntax (inc-index stx)
  (datum->syntax stx '(include-template "./template/page_index.html")))
(define-syntax (inc-esp32 stx)
  (datum->syntax stx '(include-template "./template/page_esp32.html")))
(define-syntax (inc-login stx)
  (datum->syntax stx '(include-template "./template/page_login.html")))
(define-syntax (inc-search stx)
  (datum->syntax stx '(include-template "./template/page_search.html")))
(define-syntax (inc-test stx)
  (datum->syntax stx '(include-template "./template/page_test.html")))

(define-syntax (inc-navbar stx)
  (datum->syntax stx '(include-template "./template/navbar_1.html")))
(define-syntax (inc-carousel stx)
  (datum->syntax stx '(include-template "./template/carousel_1.html")))
(define-syntax (inc-template stx)
  (datum->syntax stx '(include-template "./template/template.html")))

(def ip "还没有获取IP")
(def (render/response-esp32 request)
  (render/response-with-template (inc-esp32) request))

(def (render/response-index request)
  (render/response-with-template (inc-index) request #:carousel? #t))

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
          (render/response-with-template (inc-login) request))
        ; 登录成功，跳转回主页
        ; 这里花了一晚上的时间调试都没有结果，提交表单时怎么也无法跳转到
        ; 这段代码，结果第二天继续DEBUG时发现，原来程序并没有出错！！！
        ; 是Firefox擅自把Cached的数据作为跳转页面，直接跳回主页了！！！
        ; 清除Firefox缓存就好了%>_<%
        ;(display "COOKIE HEADER: ")
        ;(response/xexpr "HELLO")
        ;(println (cookie->header
        ;(make-login-cookie username userpwd)))
        (redirect-to "/" permanently #:headers
                     (list (cookie->header
                             (make-login-cookie username userpwd))))))
    (render/response-with-template (inc-login) request)))
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

  (let ([navbar (inc-navbar)]
        [carousel (if carousel? (inc-carousel) "")])
    (page (response/xexpr #:cookies empty
                          `(html ,(make-cdata #f #f (inc-template)))))))
; }}}

; test {{{

(def (render/response-test request path)
  (cond 
    [(= (length path) 0)
     (response/xexpr
       (let ([content (inc-test)])
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
    ; 负责人机交战的逻辑 {{{
    ; start 负责开启engine
    [(and (string=? (car path) "start")
          (= (len path) 3))
     (let
       ([player_name (cadr path)]
        [engine_name (string->symbol (caddr path))])
       (if (not eng1)
         (set-engine! (init-engine engine_name))
         (when (not (eq? engine_name (engine-name eng1)))
           (print engine_name)
           (print (engine-name eng1))
           ((engine-ctrl eng1) 'kill)
           (set-engine! (init-engine engine_name))))
       (if (not current-player)
         (begin
           (set! current-player player_name)
           (response/xexpr "start ok"))
         (response/xexpr
           (string-append "unfinished play! player_name:" current-player))))]

    [(and (string=? (car path) "play")
          (= (len path) 2))
     (if (not eng1)
       (response/xexpr "no engine")
       (let ([n (next-move (cadr path) eng1)]
             [board (with-output-to-string
                      (λ () (print (find-board (engine-err eng1)))))])
         (response/xexpr (string-append n board))))]

    [(and (string=? (car path) "clear_board")
          (= (len path) 1))
     (if (not eng1)
       (response/xexpr "no engine")
       (begin
         (write-gtp "clear_board" eng1)
         (when eng2 (write-gtp "clear_board" eng2))
         (let ([ret1 (read-gtp eng1)]
               [ret2 (read-gtp eng2)])
           (if (and (string=? "= " ret1) (string=? "= " ret2))
             (response/xexpr "clear_board ok")
             (response/xexpr (string-append ret1 ";" ret2))))))]

    ; stop 负责关闭engine
    [(and (string=? (car path) "stop")
          (= (len path) 1))
     (when eng1
       (write-gtp "quit" eng1)
       (read-gtp eng1)
       (set! eng1 #f))
     (when eng2
       (write-gtp "quit" eng2)
       (read-gtp eng2)
       (set! eng2 #f))
     (set! current-player #f)
     (response/xexpr "stop ok")]
    ; }}}
    ; 负责机器与机器交战的逻辑 {{{
    ; 由于机器与机器交战不需要复杂的逻辑（只需要你一步我一步地下棋）
    ; 所以可以直接将url直接解析成gtp协议的命令（空格替换成斜杠）
    [(and (string=? (car path) "startCvC")
          (= (len path) 3))
     (let ([eng1-name (string->symbol (cadr path))]
           [eng2-name (string->symbol (caddr path))])
       (when eng1 (write-gtp "quit" eng1) (read-gtp eng1))
       (when eng2 (write-gtp "quit" eng2) (read-gtp eng2))
       (if (eq? eng1-name eng2-name)
         (set! eng1 (init-engine eng1-name))
         (begin
           (set! eng1 (init-engine eng1-name))
           (set! eng2 (init-engine eng2-name))))

       (response/xexpr "startCvC ok"))]

    ; 拓展了gtp协议, genmove会附带棋盘的信息(find-board的返回值)
    [(string=? (car path) "gtp")
     (let* ([eng_name (string->symbol (cadr path))]
            [eng_ (if (eq? eng_name (engine-name eng1))
                    eng1
                    (if (eq? eng_name (engine-name eng2))
                      eng2
                      #f))]
            [cmd (string-append
                   (caddr path)
                   (apply string-append
                          (map (λ (s) (string-append " " s))
                               (cdddr path))))]
            [resp (begin 
                    (display "Writing gtp to ")  (displayln (engine-name eng_))
                    (println cmd)
                    (write-gtp cmd eng_)
                    (read-gtp eng_))]
            [board (if (string=? "genmove" (caddr path))
                     (with-output-to-string
                       (λ () (write-gtp "showboard" eng_) (read-gtp eng_)
                         (print (find-board (engine-err eng_)))))
                     "")])
       (if resp (response/xexpr (string-append "gtp:" resp board))
         (response/xexpr "gtp error")))]))
; }}}
; }}}


(def (make-login-cookie name pwd)
  (make-cookie
    name pwd
    #:expires (seconds->date (+ (current-seconds) (* 60 60 24 15)))))

(def (urlopen url)
  (define-values
    (status headers port)
    (http-sendrecv/url (string->url url)
                       #:method "GET"))
  (begin0
    (read port)
    (close-input-port port)))

(def (start request)
  (dispatch request))

(module* main #f

  (initial-db)

  (unless (directory-exists? "tmp")
    (make-directory "tmp"))
  (unless (directory-exists? "log")
    (make-directory "log"))

  ;(current-error-port (open-output-file "log/error.log" #:exists 'append))
  ;(current-output-port (open-output-file "log/output.log" #:exists 'append))
  (def request-output (open-output-file "log/request.log" #:exists 'append))

  (process "features/filter_request_log.py")

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
