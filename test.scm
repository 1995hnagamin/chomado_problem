(load "./chomado.scm")

(define disp (lambda (x) '()))
(define n 10)
(define (assoc-inclement alist key)
    (let ((v (assoc key alist)))
        (assoc-set alist key (+ 1 (if v (cdr v) 0)))))

(define (test)
  (let ((alist '()))
    (for-each (lambda (answer)
                (let ((n (chomado answer)))
                  (display answer)
                  (display ":")
                  (print n)
                  (set! alist (assoc-inclement alist n))))
              (repeat-perm n '(1 2 3 4)))
    alist))

(define result (test))
(print result)
(print 
  (exact->inexact
    (/ (fold (lambda (pair sum)
                   (+ sum (* (car pair) (cdr pair))))
             0
             result)
       (expt 4 n))))
