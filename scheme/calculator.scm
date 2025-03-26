#!/usr/bin/env guile
-e main -s
!#

(use-modules (ice-9 rdelim))

(define (remove-spaces str)
  (list->string
   (filter (lambda (c) (not (char=? c #\space)))
           (string->list str))))

(define (parse-expression expr)
  (let ((clean-expr (remove-spaces expr)))
    (cond
     ((string=? clean-expr "")
      (cons 0 "Boş ifade"))
     
     ((string->number clean-expr)
      (cons (inexact->exact (floor (string->number clean-expr))) #f))
     
     ((and (> (string-length clean-expr) 1)
           (char=? (string-ref clean-expr 0) #\()
           (char=? (string-ref clean-expr (- (string-length clean-expr) 1)) #\)))
      (parse-expression (substring clean-expr 1 (- (string-length clean-expr) 1))))
     
     (else
      (let* ((op-pos (find-lowest-precedence-op clean-expr))
             (op (and op-pos (string-ref clean-expr op-pos))))
        (if (not op-pos)
            (cons 0 "İfade anlaşılamadı")
            (let* ((left-expr (substring clean-expr 0 op-pos))
                   (right-expr (substring clean-expr (+ op-pos 1)))
                   (left-result (parse-expression left-expr))
                   (right-result (parse-expression right-expr)))
              (cond
               ((cdr left-result) left-result)
               ((cdr right-result) right-result)
               ((and (char=? op #\/) (= (car right-result) 0))
                (cons 0 "Sıfıra bölme hatası"))
               ((char=? op #\+)
                (cons (+ (car left-result) (car right-result)) #f))
               ((char=? op #\-)
                (cons (- (car left-result) (car right-result)) #f))
               ((char=? op #\*)
                (cons (* (car left-result) (car right-result)) #f))
               ((char=? op #\/)
                (cons (quotient (car left-result) (car right-result)) #f))
               (else
                (cons 0 "Bilinmeyen operatör"))))))))))

(define (find-lowest-precedence-op expr)
  (let loop ((i 0) (depth 0) (lowest-precedence 999) (lowest-op-pos #f))
    (if (>= i (string-length expr))
        (if (= depth 0)
            lowest-op-pos
            #f)
        (let ((c (string-ref expr i)))
          (cond
           ((char=? c #\()
            (loop (+ i 1) (+ depth 1) lowest-precedence lowest-op-pos))
           ((char=? c #\))
            (if (> depth 0)
                (loop (+ i 1) (- depth 1) lowest-precedence lowest-op-pos)
                #f))
           ((and (or (char=? c #\+) (char=? c #\-))
                 (= depth 0)
                 (> i 0)
                 (let ((prev-c (string-ref expr (- i 1))))
                   (or (char-numeric? prev-c)
                       (char=? prev-c #\))))
                 (<= 1 lowest-precedence))
            (loop (+ i 1) depth 1 i))
           ((and (or (char=? c #\*) (char=? c #\/))
                 (= depth 0)
                 (<= 2 lowest-precedence))
            (loop (+ i 1) depth 2 i))
           (else
            (loop (+ i 1) depth lowest-precedence lowest-op-pos)))))))

(define (calculator-loop)
  (display "-> ")
  (let ((input (read-line)))
    (cond
     ((string=? input "exit") 
      (newline))
     (else
      (let ((result (parse-expression input)))
        (if (cdr result)
            (format #t "Hata: ~a\n" (cdr result))
            (format #t "= ~a\n" (car result))))
      (calculator-loop)))))

(display "Çıkmak için 'exit' yazın\n")
(calculator-loop) 