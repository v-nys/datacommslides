#lang racket
(require
  pict
  (for-syntax syntax/parse))

(define (messages-per-pair->hash lst)
  (define (aux lst acc)
    (match lst
      [(list) acc]
      [(list-rest (list s r m) rest)
       (let* ([sorted-key (sort (list s r) string<?)]
              [ex-val (hash-ref acc sorted-key (list))]
              [new-acc (hash-set acc sorted-key (cons m ex-val))])
         (aux rest new-acc))]))
  (aux lst (hash)))
(module+ test
  (require rackunit)
  (check-equal?
   (messages-per-pair->hash (list))
   (hash))
  (check-equal?
   (hash->list
    (messages-per-pair->hash
     (list (list "A1" "A2" "msg1"))))
   '((("A1" "A2") "msg1")))
  (check-equal?
   (sort
    (hash->list
     (messages-per-pair->hash
      (list
       (list "A1" "A2" "msg1")
       (list "A1" "A3" "msg2")
       (list "A1" "A2" "msg3"))))
    (match-lambda**
     [((cons (list a1 a2) msg1) (cons (list a3 a4) msg2))
      (or (string<? a1 a3)
          (and (string=? a1 a3) (string<? a2 a4)))]))
   ;; messages are prepended as list is traversed, so msg3 before msg1
   ;; key is consed with a value, hence no parens around msg3 and msg1
   '((("A1" "A2") "msg3" "msg1")
     (("A1" "A3") "msg2"))))

(define (compute-spacing actors actor-rep-func msgs-per-pair msg-rep-func)
  (define (add-longest-message pair hash-acc)
    (let ([messages (hash-ref msgs-per-pair pair '())])
      (hash-set hash-acc pair (apply max (cons 0 (map (compose pict-width msg-rep-func) messages))))))
  (define (aux gap hash-acc)
    (let ([gap-separated-pairs
           (for/list ([a1 actors] [a2 (drop actors gap)])
             (list a1 a2))])
      (foldl add-longest-message hash-acc gap-separated-pairs)))
  (foldl aux (hash) (range 1 (length actors))))
(module+ test
  (let ([actor-rep (compose frame text)])
    (check-equal?
     (compute-spacing
      '("A1" "A2" "A3" "A4")
      actor-rep
      (hash
       '("A1" "A2") (list "msg1")
       '("A1" "A3") (list "msg2")
       '("A1" "A4") (list "msg3"))
      text)
     (hash '("A1" "A2") (pict-width (text "msg1"))
           '("A1" "A3") (+ (pict-width (actor-rep "A2")) (pict-width (text "msg1")))
           '("A1" "A4") (+ (pict-width (actor-rep "A2")) (pict-width (actor-rep "A3")) (pict-width (text "msg1")))))))

(define-syntax (communication-timeline stx)
  (syntax-parse stx
    [(_ (actor label) ...+
        (sender receiver msg) ...+)
     #'(let* ([actor label] ...
              [actor-list (list actor ...)]
              [actor-rep-func (compose frame text)]
              [msgs-per-pair
               (messages-per-pair->hash
                (list (list sender receiver msg) ...))]
              [msg-rep-func text]
              [required-spacing
               (compute-spacing
                actor-list actor-rep-func
                msgs-per-pair msg-rep-func)])
         (displayln "not implemented yet"))]))

(communication-timeline
 ; first: actors, groups of identifier and label
 (sender "zender")
 (receiver1 "ontvanger 1")
 (receiver2 "ontvanger 2")
 ; then: chronological messages, groups of sender + receiver + label
 (sender receiver1 "query 1")
 (receiver1 sender "response 1")
 (sender receiver2 "query 2")
 (receiver2 sender "response 2")
 (sender receiver1 "query 3")
 (receiver1 sender "response 3"))