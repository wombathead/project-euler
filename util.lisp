;;;; util.lisp

(ql:quickload :alexandria)
(ql:quickload :str)

;;; general

(defun read-from-file (filename)
  (with-open-file (s filename)
    (loop for line = (read-line s nil)
          while line
          collect line)))

(defun partition (window-size list)
  "Split LIST into sublists of size WINDOW-SIZE"
  (loop for w on list by (lambda (list) (nthcdr window-size list))
        collect (subseq w 0 window-size)))

(defun print-hash-table (ht)
  (maphash (lambda (k v) (format t "~A: ~A~%" k v)) ht))

(defun binary-search (list value)
  (loop with n = (length list)
        with a = (make-array (list n) :initial-contents list) 
        with l = 0 and h = (1- n)
        for m = (floor (+ l (/ (- h l) 2)))
        until (> l h)  
        if (= value (aref a m))
        return m
        else do (if (< value (aref a m))
                    (setf h (1- m))
                    (setf l (1+ m)))))

(defun integralp (n)
  "Return (floor N) if N represents an integer otherwise NIL"
  (when (= (floor n) (ceiling n))
    (floor n)))

(defun increasingp (list) (every #'<= list (rest list)))
(defun decreasingp (list) (every #'>= list (rest list)))

(defun select-random (list)
  (nth (random (length list)) list))

;;; dates

(defun leap-year-p (year)
  (zerop (mod year 4)))

(defun day-of-week (year month day)
  "Calculate the day of the week on which YEAR-MONTH-DAY falls in the Gregorian calendar"
  (labels ((century-anchor (c)
             (mod (+ 2 (* 5 (mod c 4)) 7) 7))
           (year-anchor (y)
             (let* ((c (floor y 100))
                    (y (mod y 100))
                    (subexp (/ (+ y (* 11 (mod y 2))) 2)))
               (mod (+ (century-anchor c)
                       (- 7 (mod (+ subexp (* 11 (mod subexp 2))) 7)))
                    7))))

    (let ((a (year-anchor year))
          (diff (cond ((= 1 month) (- day (if (leap-year-p year) 4 3)))
                      ((= 2 month) (- day (if (leap-year-p year) 29 28)))
                      ((evenp month) (- day month))
                      ((= month 3) (- day 14))
                      ((= month 5) (- day 9))
                      ((= month 7) (- day 11))
                      ((= month 9) (- day 5))
                      ((= month 11) (- day 7))
                      (t 0))))
      (mod (+ a (mod diff 7)) 7))))

(defun days-in-month (month year)
  "Number of days in MONTH for YEAR"
  (case month
    (1 31)
    (2 (if (leap-year-p year) 29 28))
    (3 31)
    (4 30)
    (5 31)
    (6 30)
    (7 31)
    (8 31)
    (9 30)
    (10 31)
    (11 30)
    (12 31)))

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

(defun pandigitalp (n)
  (loop with appeared = (make-hash-table)
        for d across (format nil "~D" n)
        do (setf (gethash d appeared) t)
        finally (return (loop for i from 0 below 9
                              for d = (code-char (+ i (char-code #\1)))
                              unless (gethash d appeared)
                              return nil
                              finally (return t)))))

(defun list-pandigital-p (list)
  (subsetp '(1 2 3 4 5 6 7 8 9) list))

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

(defun fib (n)
  "Return the Nth (zero-indexed) Fibonacci number: 0, 1, 1, 2, 3, 5, ..."
  (let* ((φ (/ (1+ (sqrt 5)) 2))
         (ψ (- 1 φ)))
    (floor (- (expt φ n) (expt ψ n))
           (sqrt 5))))

(defun miller-rabin (n k)
  "Output NIL if K rounds of Miller-Rabin determine N to be composite, otherwise T for (probably) prime"
  (when (> n 3)
    ;; TODO: come up with better decompositions n = 2^r · d + 1
    ;; (we want to find small r to minimise iterations of inner-loop)
    (let* ((r (loop for m = (1- n) then (floor m 2)
                    for r from 0
                    while (zerop (mod m 2))
                    finally (return r)))
           (d (/ (1- n) (expt 2 r))))

      (loop named witness-loop
            repeat k
            for a = (+ 2 (random (- n 3)))    ; a in U[2,n-2]
            for x = (mod (expt a d) n)
            unless (or (= x 1) (= x (1- n)))
            do (loop named inner-loop
                     repeat (1- r)
                     do (setf x (mod (* x x) n))
                     if (= x (1- n))
                     do (return-from inner-loop)
                     finally (return-from witness-loop nil))
            finally (return-from witness-loop t)))))

(defun primep (n)
  (case n
    (1 nil)
    (2 t)
    (3 t)
    (t (miller-rabin n 20))))

(defun relatively-prime-p (a b)
  (= 1 (gcd a b)))

(defun collatz (n)
  "Collatz function"
  (if (evenp n) (/ n 2) (1+ (* 3 n))))

(defun polynomial (&rest coeffs)
  "Returns the polynomial p(x)"
  (flet ((compute-polynomial (x &rest coeffs)
           "Compute p(n) = a_n x^n + ... + a_1 x + a_0 where COEFFS = '(a_n ... a_1 a_0)"
           (loop for a in coeffs
                 for p = a then (+ (* p x) a)
                 finally (return p))))

    (lambda (x) (apply #'compute-polynomial x coeffs))))

(defun lof-polynomial (&rest coeffs)
  "Returns polynomial p(x) where coeffs = '(a_0 a_1 ... a_n) are given lower order first"
  (apply #'polynomial (reverse coeffs)))

(defun evaluate-polynomial (x p)
  "Evaluate polynomial p(x) = a_n x^n + ... + a_1 x + a_0"
  (funcall p x))

(defun pollard-rho (n g)
  "Use Pollard's rho algorithm to find a non-trivial factor of N using polynomial G (mod N)"
  (let ((g (lambda (x)
             (mod (evaluate-polynomial x g) n))))

    (if (evenp n)
        2
        (loop with x = 2 and y = 2 and d = 1
              while (= d 1)
              do (setf x (funcall g x)
                       y (funcall g (funcall g y))
                       d (gcd (abs (- x y)) n))
              finally (when (/= d n) (return d))))))

(defun factors (n)
  "Return all factors of N"
  (loop for i from 1 upto (isqrt n)
        for (q r) = (multiple-value-list (floor n i))
        if (zerop r)
        if (/= i q) append (list i q)
        else append (list q)))

(defun prime-factors (n &optional probabilistic)
  "Return the prime factorization of N as a list"
  (if probabilistic 
      (when (plusp n)
        (loop for m = n then (/ m k)
              for k = (pollard-rho m (polynomial 1 0 1))
              collect k  
              until (= k m))) 
      (remove-if-not #'primep (factors n))))

(defun radical (n)
  "Return the product of N's prime factors"
  (reduce #'* (prime-factors n)))

(defun radical-multiples (n max)
  "Generate multiples of N not exceeding MAX with the same radical number"
  (labels ((generate-multiples (number factors)
             (when (<= number max)
               (cons number (loop for f in factors
                                  and r = factors then (rest r)
                                  nconc (generate-multiples (* number f) r))))))
    (generate-multiples n (prime-factors n))))

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
        with n = (array-dimension adjmatrix 0) and m = (array-dimension adjmatrix 1)
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
          initially (setf (gethash (select-random (alexandria:hash-table-keys adjlist)) tree) nil)
          for min-edge = (loop with w-min and edge
                           for u in (alexandria:hash-table-keys tree)
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
          until (= (length (alexandria:hash-table-keys tree)) (length (alexandria:hash-table-keys adjlist)))
          finally (return tree))))

(defun total-edge-weight (adjlist)
  "Sum of all edge weights in ADJLIST"
  (loop for u in (alexandria:hash-table-keys adjlist)
        sum (loop for (v . w) in (gethash u adjlist)
                  sum w) into total-weight
        finally (return (/ total-weight 2))))

(defun dijkstra (adjlist start finish)
  (flet ((dequeue (Q visited distance)
           (loop with d = sb-ext:long-float-positive-infinity
                 and v
                 for u in Q
                 if (and (< (gethash u distance) d)
                         (not (gethash u visited)))
                 do (setf d (gethash u distance)
                          v u)
                 finally (return v))))

    (loop with test = (hash-table-test adjlist)
          with visited = (make-hash-table :test test)
          with distance = (make-hash-table :test test)
          with predecessor = (make-hash-table :test test)
          with Q = (alexandria:hash-table-keys adjlist)
          initially
          (loop for u in (alexandria:hash-table-keys adjlist)
            do (setf (gethash u visited) nil
                     (gethash u distance) sb-ext:long-float-positive-infinity
                     (gethash u predecessor) nil))
          (setf (gethash start distance) 0)
          for u = (dequeue Q visited distance)
          until (funcall test u finish)
          do (loop for (v . w) in (gethash u adjlist)
               if (< (+ w (gethash u distance)) (gethash v distance))
               do (setf (gethash v distance) (+ w (gethash u distance))
                        (gethash v predecessor) (cons u w))
               do (setf (gethash u visited) t)
               if (funcall test u finish)
               return predecessor)

          finally (return predecessor))))

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
