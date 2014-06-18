(define (hamming-help a b c)
  (cond
    ((null? a) c)
    ((= (car a) (car b)) (hamming-help (cdr a) (cdr b) c))
    (else (hamming-help (cdr a) (cdr b) (+ 1 c)))))

(define (hamming a b)
  (hamming-help a b 0))

(define (assoc-set alist key val)
  (cond
    ((null? alist) (list (cons key val)))
    ((equal? key (caar alist)) (cons (cons key val) (cdr alist)))
    (else (cons (car alist) (assoc-set (cdr alist) key val)))))

(define (assoc-cons alist key val)
  (let ((v (assoc key alist)))
  (assoc-set alist key (cons val (if v (cdr v) '())))))

(define (hm-eqrel-help alist vec vecs)
  (if (null? vecs)
    alist
    (let ((nkey (hamming vec (car vecs))))
      (hm-eqrel-help (assoc-cons alist nkey (car vecs))
                     vec
                     (cdr vecs)))))

(define (hm-alist vec vecs)
  (hm-eqrel-help '() vec vecs))

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
  (max-length (hm-eqrel vec vecs)))

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

(define (answer f candidates nums-of-questions trial)
  (let* ((cand (if (= trial 1)
                 (iota nums-of-questions 1 0)
                 (most-favorable-dividor candidates)))
         (score (f cand)))
    (if (= nums-of-questions score)
      trial
      (answer f
              (filter (lambda (vec) (= (- nums-of-questions score)
                                       (hamming cand vec)))
                      candidates)
              nums-of-questions
              (+ 1 trial)))))

(define (disp something)
  (display something))

(define (akapen answer)
  (let ((n (length answer)))
    (lambda (student)
      (disp student)
      (disp ": ")
      (disp (- n (hamming student answer)))
      (disp "\n")
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
          1))
