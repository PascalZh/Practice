#!/usr/bin/env racket
#lang racket

(require "../libcommon.rkt")
(require "manipulate_db.rkt")
(require web-server/http/bindings web-server/templates)

(provide render-admin-login-content)
(provide render-search-page render-search-items)

; return html as a string
(def (render-search-page request)
  (let* ([search-name (extract-binding/single
                        'search
                        (request-bindings request))]
         [search-html (render-search-items
                        search-name
                        (query-fortune))])
    (include-template "./template/page_search.html")))

(def (render-search-items search-name query-result)

  (def (render-fortune)
    (css-search
      (css-search-item
        "<h4>在服务器上运行命令：</h4><div class=\"table-responsive\"><table class=\"table table-bordered table-hover\">"
        "<thead><tr><td>索引</td><td>运行的命令</td><td>结果</td></tr></thead>"
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
                        "<td><pre>"
                        (vector-ref vect 2)
                        "</td></pre>"
                        "</tr>"))
                    query-result))
        "</tbody>"
        "</table></div>")))

  (render-fortune))

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

(def (render-admin-login-content)
  (string-append
          "<li class=\"nav-item dropdown\">"
          "<a class=\"nav-link dropdown-toggle\""
          "href=\"#\" id=\"navbarDropdown\" role=\"button\""
          "data-toggle=\"dropdown\" aria-haspopup=\"true\" aria-expanded=\"false\">"
          "<img id=\"admin_img\" src=\"/images/admin.jpg\" height=\"20px\"/></a>"
          "<div class=\"dropdown-menu\" aria-labelledby=\"navbarDropdown\">"
          "<a class=\"dropdown-item\" href=\"javascript:onLogout()\">"
          "<strong>Logout</strong></a></div></li>"))
