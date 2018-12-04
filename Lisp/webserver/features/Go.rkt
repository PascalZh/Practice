#!/usr/bin/env racket
#lang racket
(require "../../libcommon.rkt")

(provide
  (contract-out
    [init-engine (-> symbol? engine?)]
    ;; 暂时不考虑时间的问题，根据这一步的来计算下一步
    ; 每一步用一个符号表示，对应GTPv2协议
    ; wA2 对应GTPv2的'w A2'命令，但是符号要求要比GTPv2严格
    ; next-move 会自动调用show_board
    [next-move (string? engine? . -> . string?)]
    [write-gtp (string? engine? . -> . any)]
    [read-gtp (engine? . -> . string?)]
    [struct engine ((name symbol?)
                    (in input-port?)
                    (out output-port?)
                    (err input-port?)
                    (pid integer?)
                    (ctrl procedure?))]
    [find-board (input-port? . -> . list?)]))

(module+ test
  (require rackunit))

(struct engine (name in out err pid ctrl) #:transparent)

(def (init-engine name)
  ;; 手动建立管道, p_in/p_out 代表子进程的stdin/stdout
  (define-values (p_in out) (make-pipe))
  (define-values (in p_out) (make-pipe))
  (define-values (err p_err) (make-pipe))
  (def leelaz-path
    (string-append
      "~/program_files/leela-zero/build/leelaz "
      "--cpu-only -w ~/program_files/leela-zero/build/best-network "
      "--gtp -t 4 --noponder -p 1"))
  (def AGo-path
    (string-append "~/program_files/AGo/AGo"))
  (cond [(eq? name 'leelaz)
         (displayln "init engine leela zero...")
         (def l (process/ports p_out p_in p_err leelaz-path))
         (engine 'leelaz in out err (list-ref l 2) (list-ref l 4))]
        [(eq? name 'AGo)
         (displayln "init engine AGo...")
         (def l (process/ports p_out p_in p_err AGo-path))
         (engine 'AGo in out err (list-ref l 2) (list-ref l 4))]
        ))

(def (next-move coordinate eng)
  (def ret #f)
  (when (eq? 'running ((engine-ctrl eng) 'status))
    (let*
      ([str coordinate]
       [color (string (string-ref str 0))]
       [coord  (substring str 1)]
       [cmd (string-append "play " color " " coord)]
       [color-rev (if (string=? color "w") "b" "w")])

      (write-gtp cmd eng)
      (def r1 (read-gtp eng))
      (if (or (string=? "?" (string (string-ref r1 0)))
              (not (string=? "= " r1)))
        (set! ret (substring r1 2))
        (begin
          (write-gtp (string-append "genmove " color-rev) eng)
          (set! ret (substring (read-gtp eng) 2)))))
    (write-gtp "showboard" eng)
    (read-gtp eng)
    ret))

(module+ test
  (def eng (init-engine 'leelaz))
  (LOGI "test write-gtp" "play w A1")
  (write-gtp "play w A1" eng)
  (LOGI "test read-gtp" (read-gtp eng))
  (LOGI "test next-move" (next-move eng 'bA2 ))
  (write-gtp "quit" eng)
  (read-gtp eng)

  (set! eng (init-engine 'AGo))
  (LOGI "AGo" (write-gtp "play w A1" eng))
  (LOGI "AGo" (read-gtp eng))
  (LOGI "AGo" (write-gtp "showboard" eng))
  (LOGI "AGo" (read-gtp eng))
  (LOGI "AGo stderr" (read (engine-err eng)))
  )

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

(def (find-board err)
  ; parse-board-line 返回一行的旗子
  ; 第一个为纵坐标，其他的为横坐标
  ; '(12 ('B 1 3) ('W 4 5) ('current 4))
  (def (parse-board-line n)
    (let ([l (read-line err)])
      (list (- 19 n)
            (cons 'B (map (λ (pair) (((car pair) . - . 1) . / . 2))
                          (regexp-match-positions* #rx"X" l)))
            (cons 'W (map (λ (pair) (((car pair) . - . 1) . / . 2))
                          (regexp-match-positions* #rx"O" l)))
            (cons 'current (map (λ (pair) ((car pair) . / . 2))
                                (regexp-match-positions* #rx"\\(" l))))))
  (def (loop)
    (let ([l (read-line err)])
      (if (regexp-match #rx"a b c d e" l)
        (let ([ret (build-list 19 parse-board-line)])
          (when (regexp-match #rx"a b c d e" (read-line err))
            ret))
        (loop))))
  (loop))
