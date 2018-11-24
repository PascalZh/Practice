#!/usr/bin/env racket
#lang racket
(require "../../libcommon.rkt")

(provide
  (contract-out
    [init-engine (-> symbol? engine?)]
    ;; 暂时不考虑时间的问题，根据这一步的来计算下一步
    ; 每一步用一个符号表示，对应GTPv2协议
    ; wA2 对应GTPv2的'w A2'命令，但是符号要求要比GTPv2严格
    [next-move (string? engine? . -> . string?)]
    [write-gtp (string? engine? . -> . any)]
    [read-gtp (engine? . -> . string?)]))

(module+ test
  (require rackunit))

(struct engine (in out err pid ctrl))

(def (init-engine name)
  (cond [(eq? name 'leelaz)
         ;; 手动建立管道, p_in/p_out 代表子进程的stdin/stdout
         (define-values (p_in out) (make-pipe))
         (define-values (in p_out) (make-pipe))
         (define-values (err p_err) (make-pipe))
         (displayln "init engine leela zero...")
         (def l (process/ports p_out p_in p_err
                               "~/program_files/leela-zero/build/leelaz --cpu-only -w ~/program_files/leela-zero/build/best-network --gtp -t 4 --noponder -p 1"))
         (engine in out err (list-ref l 2) (list-ref l 4))]))

(def (next-move coordinate eng)
  (when (eq? 'running ((engine-ctrl eng) 'status))
    (let*
      ([str coordinate]
       [color (string (string-ref str 0))]
       [coord  (substring str 1)]
       [cmd (string-append "play " color " " coord)]
       [color-rev (if (string=? color "w") "b" "w")])
      
      (write-gtp cmd eng)
      (def r1 (read-gtp eng))
      (if (string=? "?" (string (string-ref r1 0)))
        r1
        (begin
          (write-gtp (string-append "genmove " color-rev) eng)
          (string-append color-rev
                         (substring (read-gtp eng) 2)))))))

(module+ test
  (def eng (init-engine 'leelaz))
  (LOGI "test write-gtp" "play w A1")
  (write-gtp "play w A1" eng)
  (LOGI "test read-gtp" (read-gtp eng))
  (LOGI "test next-move" (next-move eng 'bA2 )))

(def (read-gtp eng)
  (when (eq? 'running ((engine-ctrl eng) 'status))
    (let ([l1 (read-line (engine-in eng))]
          [l2 (read-line (engine-in eng))])
      (when (not (string=? l2 ""))
        (LOGE "read-gtp" l2))
      (when (string=? "?" (string (string-ref l1 0)))
        (LOGE "read-gtp" l1))
      l1)))
(def (write-gtp cmd eng)
  (when (eq? 'running ((engine-ctrl eng) 'status))
    (displayln cmd (engine-out eng))))
