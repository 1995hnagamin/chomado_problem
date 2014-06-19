(define (hamming-help a b c)
  (cond
    ((null? a) c)
    ((= (car a) (car b)) (hamming-help (cdr a) (cdr b) c))
    (else (hamming-help (cdr a) (cdr b) (+ 1 c)))))

(define *hamming-table* (make-hash-table 'equal?))

(define (hamming a b)
  (if (hash-table-exists? *hamming-table* (list a b))
    (hash-table-get *hamming-table* (list a b))
    (let ((val (hamming-help a b 0)))
      (hash-table-put! *hamming-table* (list a b) val)
      (hash-table-put! *hamming-table* (list b a) val)
      val)))

(define (assoc-set alist key val)
  (cond
    ((null? alist) (list (cons key val)))
    ((equal? key (caar alist)) (cons (cons key val) (cdr alist)))
    (else (cons (car alist) (assoc-set (cdr alist) key val)))))

(define (assoc-cons alist key val)
  (let ((v (assoc key alist)))
    (assoc-set alist key (cons val (if v (cdr v) '())))))

(define (assoc-increment alist key)
    (let ((v (assoc key alist)))
        (assoc-set alist key (+ 1 (if v (cdr v) 0)))))

(define (hm-eqn-help alist vec vecs)
  (if (null? vecs)
    alist
    (let ((nkey (hamming vec (car vecs))))
      (hm-eqn-help (assoc-increment alist nkey)
                   vec
                   (cdr vecs)))))

(define (hm-alist vec vecs)
  (hm-eqn-help '() vec vecs))

(define (hm-eqrel vec vecs)
  (map cdr (hm-alist vec vecs)))

(define (max-length-help lists maximum)
  (cond
    ((null? lists) maximum)
    ((< maximum (length (car lists))) (max-length-help (cdr lists)
                                                       (length (car lists))))
    (else (max-length-help (cdr lists) maximum))))

(define (max-length lists)
  (max-length-help lists 0))

(define (division vec vecs)
  (apply max (hm-eqrel vec vecs)))

(define (minimum f xs)
  (letrec ((M
             (lambda (ys mini size)
               (if (null? ys)
                 mini
                 (let ((newsize (f (car ys))))
                   (cond
                     ((> size newsize) (M (cdr ys) (car ys) newsize))
                     (else (M (cdr ys) mini size))))))))
    (M xs (car xs) (f (car xs)))))

(define (most-favorable-dividor vecs)
  (minimum (lambda (vec) (division vec vecs)) vecs))

(define *mfd-table* (make-hash-table))

(define (answer f candidates nums score trial)
  (let* ((cand (cond
                 ((= trial 1) (iota nums 1 0))
                 ((and (= trial 2) (hash-table-exists? *mfd-table* nums)) (hash-table-get *mfd-table* nums))
                 ((= trial 2)
                  (let ((val (most-favorable-dividor candidates)))
                    (hash-table-put! *mfd-table* score val)
                    val))
                 (else (most-favorable-dividor candidates))))
         (score (f cand)))
    (disp ", candidates:")
    (disp (length candidates))
    (disp "\n")
    (if (= nums score)
      trial
      (answer f
              (filter (lambda (vec) (= (- nums score)
                                       (hamming cand vec)))
                      candidates)
              nums
              score
              (+ 1 trial)))))

(define (disp something)
  (display something))

(define (akapen answer)
  (let ((n (length answer)))
    (lambda (student)
      (disp student)
      (disp ": ")
      (disp (- n (hamming student answer)))
      (- n (hamming student answer)))))

(define (repeat-perm n l)
  (if (= 1 n)
    (map (lambda (x) (list x)) l)
    (apply append
      (map (lambda (y)
             (map (lambda (x) (cons y x))
                  (repeat-perm (- n 1) l)))
           l))))


(define (chomado ans)
  (answer (akapen ans)
          (repeat-perm (length ans) (list 1 2 3 4))
          (length ans)
          0
          1))
