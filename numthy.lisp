;;;; numthy.lisp
;;;; a collection of algorithms for number theoretic problems

(defun miller-rabin (n k)
  "Return T iff N is composite using K > 3 rounds of the Miller-Rabin test"

  ;; if n is 2 then prime, 
  (cond ((< n 2) (return-from miller-rabin t)) 
        ((= n 2) (return-from miller-rabin nil))
        ((evenp n) (return-from miller-rabin t)))

  (flet ((decompose-n (n)
           (loop for r from 1 upto (floor (log n 2)) do
                 (loop for d from 1 upto (floor n (expt 2 r))
                       do (if (= (* d (expt 2 r)) (1- n))
                              (return-from decompose-n (values r d)))))))

    (multiple-value-bind (r d) (decompose-n n)
      (loop repeat k
        for a = (+ 2 (random (- n 1)))
        for x = (mod (expt a d) n)
        do (unless (or (= x 1) (= x (1- n)))
             (loop repeat (1- r)
                   for x = (mod (* x x) n)
                   do (if (= x (1- n))
                          (return)))
             (return-from miller-rabin t)))
      (return-from miller-rabin nil))))
