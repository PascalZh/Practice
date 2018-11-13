#!/usr/bin/env racket
#lang racket

(require "../libcommon.rkt")
(provide render-search-items)


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
