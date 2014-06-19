(load "./chomado.scm")

(define disp (lambda (x) '()))
(define n 10)

(define (test)
  (let ((alist '()))
    (for-each (lambda (answer)
                (let ((n (chomado answer)))
                  (display answer)
                  (display ":")
                  (print n)
                  (set! alist (assoc-increment alist n))))
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
