;;;; util.lisp

;;; general

(defun read-from-file (filename)
  (with-open-file (s filename)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun integralp (n)
  "Return T if N represents an integer"
  (= (floor n) (ceiling n)))

;;; strings

(defun digits-of (n &optional (base 10))
  "Collect the digits of N written in base BASE into a list"
  (loop for a = n then q
        for (q r) = (multiple-value-list (floor a base))
        collect r into digits
        until (zerop q)
        finally (return (reverse digits))))

(defun to-base (n base)
  (format nil "~{~S~}" (digits-of n base)))

(defun digits-to-number (digits &optional (base 10))
  "Convert list of DIGITS in base BASE to a number"
  (loop for d in digits
        for n = d then (+ (* base n) d)
        finally (return n)))

(defun palindromep (str)
  (string= str (reverse str)))

(defun number-palindrome-p (n)
  (let ((str (write-to-string n)))
    (string= str (reverse str))))

(defun digit-rotations (n)
  (loop with digits = (digits-of n)
        for d in digits
        for rotation = (append (rest digits) (list d)) then (append (rest rotation) (list d))
        collect rotation))

;;; number theory

(defun primep (n)
  (case n
    (1 nil)
    (2 t)
    (3 t)
    (t (loop for prime = t then (if (zerop (mod n i)) nil prime)
             for i from 2 upto (isqrt n)
             until (not prime)
             finally (return prime)))))

(defun collatz (n)
  "Collatz function"
  (if (evenp n) (/ n 2) (1+ (* 3 n))))

(defun pollard-rho (n &optional g)
  "Return a prime factor of N using polynomial G"
  (let* ((n (abs n))
         (g (if g g (lambda (x) (mod (+ (* x x) (random n)) n))))
         (result (loop for x = 2 then (funcall g x)
                       for y = 2 then (funcall g (funcall g y))
                       for d = 1 then (gcd (abs (- x y)) n)
                       while (= d 1)
                       finally (return d))))
    (if (= result n) n result)))

(defun prime-factors (n)
  "Return the prime factorization of N as a list"
  (when (plusp n)
    (loop for m = n then (/ m k)
          for k = (pollard-rho m)
          collect k  
          until (= k m))))

(defun factors (n)
  "Return all factors of N"
  (loop for i from 1 upto (isqrt n)
        for (q r) = (multiple-value-list (floor n i))
        if (zerop r)
        if (/= i q) append (list i q)
        else append (list q)))

(defun proper-factors (n)
  "All factors of N excluding N"
  (remove n (factors n)))

(defun perfect-number-p (n)
  (= n (reduce #'+ (proper-factors n))))

(defun eratosthenes-sieve (n &optional (return-type 'list))
  "Return list of all primes below N"
  ;; TODO: can optimise by only storing odd numbers (since no evens are prime
  ;; except for 2) and using index maths
  (loop with composites = (make-array n :initial-element nil)
        with primes-array = (make-array n :initial-element nil)
        for i = 2 then (loop for j from (1+ i) below n
                             unless (aref composites j)
                             minimize j)
        until (zerop i)
        do (loop for j from 1 upto (floor n i)
                 for idx = (* i j)
                 if (< idx n)
                 do (setf (aref composites idx) t))

        if (eq return-type 'list)
        collect i into primes-list
        else
        do (setf (aref primes-array i) t)
        finally (return (if (eq return-type 'list) primes-list primes-array))))

(defun factorial (n)
  "N! = N * (N-1) * ... * 2 * 1"
  (if (zerop n)
      1
      (loop for i from 1 upto n
            for j = i then (* j i)
            finally (return j))))

(defun choose (n k)
  "N choose K"
  (/ (factorial n) (* (factorial (- n k)) (factorial k))))

(defun binomial (x y n)
  "Compute (x + y)^n"
  (loop for k from 0 upto n
        sum (* (choose n k) (expt x k) (expt y (- n k)))))

(defun polynomial (x &rest coeffs)
  "Compute the n-degree polynomial p(n) = a_n x^n + ... + a_1 x + a_0 where COEFFS = '(a_n ... a_1 a_0)"
  (loop for a in coeffs
        for p = a then (+ (* p x) a)
        finally (return p)))

(defun quadratic-roots (a b c)
  (let ((discriminant (- (* b b) (* 4 a c))))
    (values (/ (+ (- b) (sqrt discriminant)) (* 2 a))
            (/ (- (- b) (sqrt discriminant)) (* 2 a)))))

(defun quadratic-inverse (y a b c)
  "Inverse of the quadratic function y = a x^2 + b x + c"
  ;; TODO: remove y term (incorporate into c term)
  (let ((sqrt-term (sqrt (+ (* (/ 1 a) (- y c))
                            (expt (/ b (* 2 a)) 2)))))
    (values (+ (- (/ b (* 2 a))) sqrt-term)
            (- (- (/ b (* 2 a))) sqrt-term))))

(defun triangle-n (n)
  "Compute the Nth triangle number"
  (polynomial n 1/2 1/2 0))

(defun inverse-triangle (y)
  (quadratic-inverse y 1/2 1/2 0))

(defun pentagonal-n (n)
  "Compute the Nth pentagonal number"
  (polynomial n 3/2 -1/2 0))

(defun inverse-pentagonal (y)
  (quadratic-inverse y 3/2 -1/2 0))

(defun hexagonal-n (n)
  "Compute the Nth hexagonal number"
  (polynomial n 2 -1 0))

(defun inverse-hexagonal (y)
  (quadratic-inverse y 2 -1 0))

;;; graphs

#|
(defun topological-ordering (G)
  ;; find a vertex with no incoming edges
  ;(let ((degrees (make-hash-table :test (hash-table-test G)))))
  )

(defun number-of-paths (G)
  ;; first order G topologically
  ;; then in reverse order: #paths(u) = sum(#paths(v) : (u,v) in E)
  )
|#
