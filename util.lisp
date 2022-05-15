;;;; util.lisp

(ql:quickload :alexandria)

;;; general

(defun read-from-file (filename)
  (with-open-file (s filename)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun integralp (n)
  "Return (floor N) if N represents an integer otherwise NIL"
  (when (= (floor n) (ceiling n))
    (floor n)))

(defun increasingp (list) (every #'<= list (rest list)))
(defun decreasingp (list) (every #'>= list (rest list)))

(defun select-random (list)
  (nth (random (length list)) list))

;;; strings

(defun first-digits-of (n d &optional (base 10))
  (let ((digits (1+ (floor (log n base)))))
    (unless (minusp d)
      ;; TODO: get this working for bases other than 10
      (format nil "~D" (floor n (expt base (max (- digits d) 0)))))))

(defun last-digits-of (n d &optional (base 10))
  ;; TODO: get this working for bases other than 10
  (format
    nil
    (format nil "~~~D,'0D" d)
    (nth-value 1 (floor n (expt base d)))))

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

(defun polynomial (&rest coeffs)
  "Returns the polynomial p(x)"
  (flet ((compute-polynomial (x &rest coeffs)
           "Compute p(n) = a_n x^n + ... + a_1 x + a_0 where COEFFS = '(a_n ... a_1 a_0)"
           (loop for a in coeffs
                 for p = a then (+ (* p x) a)
                 finally (return p))))

    (lambda (x) (apply #'compute-polynomial x coeffs))))

(defun evaluate-polynomial (x p)
  "Evaluate polynomial p(x) = a_n x^n + ... + a_1 x + a_0"
  (funcall p x))

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
  ;; T(n) = n(n+1)/2
  (evaluate-polynomial n (polynomial 1/2 1/2 0)))

(defun inverse-triangle (y)
  (quadratic-inverse y 1/2 1/2 0))

(defun pentagonal-n (n)
  "Compute the Nth pentagonal number"
  ;; P(n) = n(3n-1)/2
  (evaluate-polynomial n (polynomial 3/2 -1/2 0)))

(defun inverse-pentagonal (y)
  (quadratic-inverse y 3/2 -1/2 0))

(defun hexagonal-n (n)
  "Compute the Nth hexagonal number"
  ;; H(n) = n(2n-1)
  (triangle-n (1- (* 2 n))))

(defun inverse-hexagonal (y)
  (quadratic-inverse y 2 -1 0))

(defun bouncyp (n &optional (base 10))
  "N is bouncy in base BASE if the digits form neither an increasing nor a decreasing sequence"
  (loop with increasesp and decreasesp
        for m = n then (floor m base)
        for prev-digit = (mod m base) then digit
        for digit = (mod m base)
        until (zerop m)
        if (< digit prev-digit) do (setf increasesp t)
        if (> digit prev-digit) do (setf decreasesp t)
        if (and increasesp decreasesp) return t
        finally (return nil)))

;;; linear algebra

(defun characteristic-matrix (n)
  "Generate the matrix of constraints satisfied by the first N terms of polynomial of degree d satisfy"
  (loop with matrix = (make-array (list n n))
        for i from 0 below n
        do (loop for j from 0 below n
                 for a = (1+ i) and b = (1+ j)
                 do (setf (aref matrix i j) (expt a (- n b))))
        finally (return matrix))) 

(defun gaussian-elimination (A)
  "Perform Gaussian elimination on matrix A"
  ;; TODO: this probably only works for square matrices for now
  (let* ((n (array-dimension A 0))
         (m (array-dimension A 1))
         (B (alexandria:copy-array A)))

    ;; first get the matrix in row echelon form
    (loop for i from 0 below n
          for factor = (aref B i i)
          ;; first get the leading coefficient equal to 1
          unless (zerop factor)
          do (loop for j from i below m
                   for aij = (aref B i j) 
                   do (setf (aref B i j) (/ aij factor)))
          ;; now eliminate the leading coefficient in all rows below
          do (loop for k from (1+ i) below n
                   for leading-coefficient = (aref B k i)
                   do (loop for j from 0 below m
                            for entry = (aref B k j)
                            for delta = (* leading-coefficient (aref B i j))
                            do (setf (aref B k j) (- entry delta)))))

    ;; now get the matrix in reduced row echelon form
    (loop for i from (1- n) downto 0
          do (loop for k from (1- i) downto 0
                   for factor = (aref B k i)
                   do (loop for j from i below m
                            for entry = (aref B k j)
                            for delta = (* factor (aref B i j))
                            do (setf (aref B k j) (- entry delta)))))
    B)) 

(defun augmented-matrix (A b)
  "Splice together the nxm array A with the n vector B"
  (let* ((n (array-dimension A 0))
         (m (array-dimension A 1))
         (augmented (make-array (list n (1+ m)))))

    (loop for i from 0 below n
          do (loop for j from 0 below m
                   do (setf (aref augmented i j) (aref A i j))))

    (loop for i from 0 below n
          do (setf (aref augmented i m) (aref b i)))

    augmented))

(defun solve-linear-system (A b)
  "Solve linear system of equations defined by Ax = b"
  (loop with gaussian = (gaussian-elimination (augmented-matrix A b))
        with n = (array-dimension gaussian 0) and m = (array-dimension gaussian 1) 
        with solution = (make-array n)
        for i from 0 below n
        do (setf (aref solution i) (aref gaussian i (1- m)))
        finally (return solution)))

;;; geometry

(defun vector-add (u v)
  (map 'vector #'+ u v))

(defun vector-minus (u v)
  (map 'vector #'- u v))

(defun lp-norm (p u)
  (expt (reduce #'+ (map 'vector (lambda (ui) (expt (abs ui) p)) u))
        (/ 1 p)))

(defun dot (u v)
  "Dot product of U and V"
  (reduce #'+ (map 'vector (lambda (ui vi) (* ui vi)) u v)))

(defun cross (u v)
  "Vector cross product of U and V"
  (let ((u1 (aref u 0)) (u2 (aref u 1)) (u3 (aref u 2))
        (v1 (aref v 0)) (v2 (aref v 1)) (v3 (aref v 2)))
    (vector (- (* u2 v3) (* u3 v2))
            (- (* u3 v1) (* u1 v3))
            (- (* u1 v2) (* u2 v1)))))

(defun triangle-contains-point-p (a b c p)
  "Return T if point P is below the halfspace defined by vector B-A"
  (let* ((a (vector-minus a p)) ; first translate triangle to P
         (b (vector-minus b p))
         (c (vector-minus c p))
         (u (cross b c))        ; normal of PBC
         (v (cross c a))        ; normal of PCA
         (w (cross a b)))       ; normal of PAB

    (not (or (minusp (dot u v))
             (minusp (dot u w))))))

;;; graphs

(defun file->adjmatrix (filename)
  "Parse FILENAME into adjacency matrix"
  (let* ((input (mapcar (lambda (line)
                          (mapcar (lambda (s)
                                    (unless (string= "-" s)
                                      (parse-integer s)))
                                  (str:split "," line)))
                        (read-from-file filename)))
         (n (length input))
         (m (length (first input))))
    (make-array (list n m) :initial-contents input)))

(defun adjmatrix->adjlist (adjmatrix)
  "Convert adjacency matrix to adjacency list"
  (loop with adjlist = (make-hash-table)
        and (n m) = (array-dimensions adjmatrix)
        for u from 0 below n
        do (loop for v from 0 below m
                 for w = (aref adjmatrix u v)
                 when (aref adjmatrix u v)
                 do (push (cons v w) (gethash u adjlist)))
        finally (return adjlist)))

(defun edge-in-adjlist-p (adjlist u v)
  "Check if edge (u,v) is in adjacency list ADJLIST"
  (let (found)
    (loop for (n . w) in (gethash u adjlist)
          if (funcall (hash-table-test adjlist) v n)
          do (setf found t)
          finally (return found))))

(defun prims (adjlist)
  "A (highly inefficient) implementation of Prim's minimum spanning tree algorithm for ADJLIST"
  (flet ((valid-edge-p (adjlist v w current-min)
           (or (null current-min)
               (and (< w current-min)
                    (null (gethash v adjlist))))))

    (loop with tree = (make-hash-table :test (hash-table-test adjlist))
          initially (setf (gethash (select-random (hash-table-keys adjlist)) tree) nil)
          for min-edge = (loop with w-min and edge
                           for u in (hash-table-keys tree)
                           do (loop for (v . w) in (gethash u adjlist)
                                    if (valid-edge-p tree v w w-min)
                                    do (setf w-min w
                                             edge (list u v w)))
                           finally (return edge))
          do
          (let ((u (first min-edge))
                (v (second min-edge))
                (w (third min-edge)))
            (push (cons v w) (gethash u tree))
            (push (cons u w) (gethash v tree)))
          until (= (length (hash-table-keys tree)) (length (hash-table-keys adjlist)))
          finally (return tree))))

(defun total-edge-weight (adjlist)
  "Sum of all edge weights in ADJLIST"
  (loop for u in (hash-table-keys adjlist)
        sum (loop for (v . w) in (gethash u adjlist)
                  sum w) into total-weight
        finally (return (/ total-weight 2))))

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
