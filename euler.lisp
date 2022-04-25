;;;; euler.lisp

(load "util.lisp")

(defun euler-001 (n)
  "Sum of multiples of 3 or 5 less than N"
  (loop for i from 1 below n
        if (or (zerop (mod i 3)) (zerop (mod i 5)))
        sum i))

(defun euler-002 (n)
  "Sum of even Fibonacci numbers not exceeding N"
  (loop for a = 0 then b
        for b = 1 then c
        for c = (+ a b)
        when (evenp c)
        sum c
        until (> c n)))

(defun euler-003 (n)
  "Computes the largest prime factor of N"
  (reduce #'max (prime-factors n)))

(defun euler-004 (n)
  "Compute largest palindrome product of two N-digit numbers"
  (let ((smallest (expt 10 (1- n)))
        (largest (1- (expt 10 n))))
    (loop for i from smallest upto largest
          maximize (loop for j from smallest upto largest
                         for product = (* i j)
                         if (number-palindrome-p product)
                         maximize product))))

(defun euler-005 (n)
  "Compute smallest number divisible by all numbers 2..N"
  (loop for a = 1 then (lcm a i)
        for i from 2 upto n
        finally (return a)))

(defun euler-006 (n)
  "Difference between sum of squares and square of sum for the first N natural numbers"
  (let ((nums (loop for i from 1 to n collect i)))
    (abs (- (expt (reduce #'+ nums) 2)
            (reduce #'+ (mapcar (lambda (x) (* x x)) nums))))))

(defun euler-007 (n)
  "Return Nth prime number"
  (if (< n 2)
      2
      (loop for i from 3 by 2
            for is-prime = (primep i)
            for p = (if is-prime i p)    
            if is-prime
            count is-prime into n-primes
            until (>= (1+ n-primes) n)
            finally (return p))))

(defun euler-008 (filename m)
  "Find the M adjacent digits in string N with the largest product"
  (loop with n = (first (read-from-file filename))
        with l = (length n) 
        for i from 0 upto (- l m)
        for window = (subseq n i (+ i m))
        maximize (reduce #'* (map 'list (lambda (c) (- (char-int c) (char-int #\0))) window))))

(defun euler-009 (n)
  "Find a Pythagorean triple whose sum is N"
  (loop for a from 1 upto n
        do (loop for b from (1+ a) upto n
                 do (loop for c from (1+ b) upto n
                          for a2 = (* a a) and b2 = (* b b) and c2 = (* c c)
                          do (if (and (= (+ a2 b2) c2)
                                      (= (+ a b c) n))
                                 (return-from euler-009 (* a b c)))))))

(defun euler-010 (n)
  "Find sum of all primes below N"
  ; (reduce #'+ (eratosthenes-sieve n)))
  (loop with composites = (make-array (list n) :initial-element nil)
        ; find minimum unmarked index in composites
        for i = 2 then (loop for j from (1+ i) 
                             until (or (= j n)
                                       (not (aref composites j)))
                             finally (return j))
        until (= i n)
        do (loop for j from 1 upto (floor n i)
                 for idx = (* i j)
                 if (< idx n)
                 do (setf (aref composites idx) 1))
        sum i))

(defun euler-012 (n)
  "First triangle number with over N divisors"
  (loop for triangle-number = 1 then (+ triangle-number i) 
        for i from 2
        until (> (length (factors triangle-number)) n)
        finally (return triangle-number)))

(defun euler-013 (filename)
  (let ((numbers (mapcar #'parse-integer (read-from-file filename))))
    (reduce #'+ numbers)))

(defun euler-014 (n)
  "Find input to Collatz function that produces the longest chain"
  (flet ((chain-length (n)
           ;; find number of iterations until C(n) reaches unity
           (loop for m = n then (collatz m) 
                 for i from 1
                 until (= m 1)
                 finally (return i))))
    (loop for i from 1 below n
          for l = (chain-length i)
          with max = 1 and max-input = 1
          do (when (> l max)
               (setf max l
                     max-input i))
          finally (return max-input))))

(defun euler-015 (m n)
  "Number of paths from corner to corner of an MxN lattice"
  (loop for k from 0 upto n
        ;; C(n k): # ways to go right; C(m m-k): # ways to go down
        sum (* (choose n k) (choose m (- m k)))))

(defun euler-016 (n)
  "Sum of digits in 2^1000"
  (reduce #'+ (digits-of n 10)))

(defun euler-020 (n)
  "Sum of digits in 100!"
  (reduce #'+ (digits-of n 10)))

(defun euler-021 (n)
  "Sum of amicable numbers below N"
  (flet ((d (n) (reduce #'+ (proper-factors n))))
    (loop for a from 1 below n
          for b = (d a)
          if (and (/= a b) (= (d b) a))
          sum a)))

(defun euler-022 (filename)
  "Total of all name scores in FILENAME"
  (flet ((char-score (c) (1+ (- (char-code c) (char-code #\A)))))
    (loop for name in (sort (read-from-file filename) #'string<)
          for i from 1
          for score = (reduce #'+ (map 'list (lambda (c) (char-score c)) name))
          sum (* i score))))

(defun euler-025 (digits)
  "Index of first Fibonacci number to contain 1000 digits"
  (loop for i from 2
        for a = 0 then b
        for b = 1 then c
        for c = (+ a b)
        for d = (1+ (floor (log c 10)))
        until (>= d digits)
        finally (return i)))

(defun euler-031-recursive (target coins)
  "Number of ways to reach TARGET using any number of coins from COINS"
  (labels ((ways (remaining max)
             ;; # ways to reach REMAINING using coins no less than MAX
             (if (zerop remaining)
                 1
                 (loop for c in coins
                       unless (or (< c max) (> c remaining))
                       sum (ways (- remaining c) c)))))
    (ways target 0)))

(defun euler-031-dp (target coins)
  (let ((ways (make-array (1+ target)))
        (n (length coins)))
    (setf (aref ways 0) 1)
    (loop for i from 0 below n
          do (loop for j from (aref coins i) upto target
                   do (setf (aref ways j) (+ (aref ways j)
                                             (aref ways (- j (aref coins i)))))))
    (aref ways target)))

(defun euler-031 (target coins)
  (euler-031-dp target coins))

(defun euler-047 (n)
  "Find the first N consecutive numbers with distinct prime factors"
  (loop for i from 2
        for factor-lists = (loop for j from i upto (+ i (1- n))
                                 collect (remove-duplicates (prime-factors j)))
        until (every (lambda (l) (= n (length l))) factor-lists)
        finally (return i)))

(defun euler-035 (n)
  "Return all circular primes below N"
  ;; TODO: only consider numbers only containing 1,3,7,9
  ;; TODO: test primality using a sieve
  (loop for i from 3 below n by 2
        count (every (lambda (d) (primep (digits-to-number d))) (digit-rotations i))
        into circular-primes
        ;; finally account for 2, which is circular
        finally (return (1+ circular-primes))))

(defun euler-036 (n bases)
  "Find sum of numbers below N that are palindromic in base 2 and base 10"
  ;; TODO: generate the palindromic numbers rather than guessing and checking
  (loop for i from 1 below n
        for representations = (mapcar (lambda (b) (to-base i b)) bases)
        if (every #'palindromep representations)
        sum i))

(defun main ()
  (format t "01: ~D~%" (euler-001 1000))
  (format t "02: ~D~%" (euler-002 4e6))
  (format t "03: ~D~%" (euler-003 600851475143))
  (format t "04: ~D~%" (euler-004 3))
  (format t "05: ~D~%" (euler-005 20))
  (format t "06: ~D~%" (euler-006 100))
  (format t "07: ~D~%" (euler-007 10001))
  (format t "08: ~D~%" (euler-008 "inputs/008.txt" 13))
  (format t "09: ~D~%" (euler-009 1000))
  (format t "10: ~D~%" (euler-010 2000000))
  (format t "12: ~D~%" (euler-012 500))
  (format t "13: ~D~%" (euler-013 "inputs/013.txt"))
  (format t "14: ~D~%" (euler-014 1000000))
  (format t "15: ~D~%" (euler-015 20 20))
  (format t "16: ~D~%" (euler-016 (expt 2 1000)))
  (format t "20: ~D~%" (euler-020 (factorial 100)))
  (format t "21: ~D~%" (euler-021 10000))
  (format t "22: ~D~%" (euler-022 "inputs/022.txt"))
  (format t "25: ~D~%" (euler-025 1000)) 
  (format t "31: ~D~%" (euler-031 200 #(1 2 5 10 20 50 100 200)))
  (format t "35: ~D~%" (euler-035 1000000))
  (format t "36: ~D~%" (euler-036 1000000 '(2 10))))

;; FIXME

(defun euler-23 ()
  (let (abundant-numbers
        abundable-numbers)
    (flet ((deficient-p (n)
             (> n (reduce #'+ (proper-factors n))))
           (abundant-p (n)
             (< n (reduce #'+ (proper-factors n)))))

      ;; first generate all abundant numbers less than 28123...
      (setf abundant-numbers (loop for i from 1 upto 16000
                               if (abundant-p i)
                               collect i))
      (setf abundable-numbers (loop with abundant-sums
                                for i in abundant-numbers
                                do (loop for j in abundant-numbers
                                         do (push (+ i j) abundant-sums))
                                finally (return (sort (remove-duplicates abundant-sums) #'<))))
      (loop for i from 1 upto 28123
            with nums = abundable-numbers
            if (= i (first nums))
            do (pop nums)
            else sum i))))
