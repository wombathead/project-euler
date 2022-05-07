;,;;; euler.lisp

(declaim (optimize (speed 3)))

(ql:quickload :iterate)
(ql:quickload :str)

(defpackage #:euler
  (:use :cl :iterate)
  (:export :main))

(in-package #:euler)

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

(defun euler-013 (filename D)
  "Return the first D digits of the sum of numbers given in FILENAME"
  (let ((numbers (mapcar #'parse-integer (read-from-file filename))))
    (first-digits-of (reduce #'+ numbers) d)))

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

(defun euler-023 ()
  "Find the sum of all positive integers which cannot be written as the sum of two abundant numbers"
  (let ((abundable-numbers (make-hash-table))
        abundant-numbers)

    (flet ((deficient-p (n)
             (> n (reduce #'+ (proper-factors n))))
           (abundant-p (n)
             (< n (reduce #'+ (proper-factors n)))))

      ;; first generate all abundant numbers less than 28123
      ;; TODO: this bound can probably be improved using some thinking...
      (setf abundant-numbers (loop for i from 1 upto 28123
                               if (abundant-p i)
                               collect i))

      ;; mark all sums of two abundant numbers
      (loop for i in abundant-numbers
            do (loop for j in abundant-numbers
                 do (setf (gethash (+ i j) abundable-numbers) t)))

      (loop for i from 1 upto 28123
            unless (gethash i abundable-numbers)
            sum i))))

(defun euler-025 (digits)
  "Index of first Fibonacci number to contain 1000 digits"
  (loop for i from 2
        for a = 0 then b
        for b = 1 then c
        for c = (+ a b)
        for d = (1+ (floor (log c 10)))
        until (>= d digits)
        finally (return i)))

(defun euler-029 (a b)
  "Number of distinct terms of x^y for 2<=x<=A and 2<=y<=B"
  (loop with terms = (make-hash-table)
        for x from 2 upto a
        do (loop for y from 2 upto b
                 do (setf (gethash (expt x y) terms) t))
        finally (return (hash-table-count terms))))

(defun euler-030 (n &optional (base 10))
  "Sum of all numbers that can be written as the sum of Nth powers of their digits"
  (flet ((determine-bound (f g)
           (loop for n from 1
                 for fn = (funcall f n) and gn = (funcall g n)
                 until (< fn gn)
                 finally (return n))))

    (loop with upper-bound = (determine-bound
                               (lambda (m) (* m (expt (1- base) n)))
                               (lambda (m) (1- (expt base m))))

          for i from 2 upto (expt base upper-bound)
          for digits = (digits-of i)
          for sum-of-powers = (reduce #'+ (mapcar (lambda (d) (expt d n)) digits))
          if (= i sum-of-powers)
          sum i)))

(defun euler-031-recursive (target coins)
  (labels ((ways (remaining max)
             ;; # ways to reach REMAINING using coins no less than MAX
             (if (zerop remaining)
                 1
                 (loop for c in coins
                       unless (or (< c max) (> c remaining))
                       sum (ways (- remaining c) c)))))
    (ways target 0)))

(defun euler-031-dp (target coins)
  (loop with ways = (make-array (1+ target) :initial-element 0) 
        initially (setf (aref ways 0) 1)
        for c across coins
        do (loop for i from c upto target
                 do (setf (aref ways i) (+ (aref ways i)
                                           (aref ways (- i c)))))
        finally (return (aref ways target))))

(defun euler-031 (target coins)
  "Number of ways to reach TARGET using any number of coins from COINS"
  (euler-031-dp target coins))

(defun euler-047 (n)
  "Find the first N consecutive numbers with distinct prime factors"
  (loop for i from 2
        for factor-lists = (loop for j from i upto (+ i (1- n))
                                 collect (remove-duplicates (prime-factors j)))
        until (every (lambda (l) (= n (length l))) factor-lists)
        finally (return i)))

(defun euler-048 (n d)
  "Find the last D digits of 1 + 2^2 + 3^3 + ... + n^n"
  (flet ((expt-sum (n)
           (reduce #'+ (mapcar (lambda (i) (expt i i)) (loop for i from 1 upto n collect i)))))
    (last-digits-of (expt-sum n) d)))

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

(defun euler-039 (upper-bound)
  "Find the parameter p <= UPPER-BOUND of a right-angled triangle maximising the number of distinct solutions, i.e. a,b,c such that a+b+c=p"
  ;; TODO: far too slow -- optimise
  (flet ((count-solutions (p)
           (loop with solutions = 0
                 for c from (floor p 3) upto p
                 do (loop for b from 1 upto c
                          do (loop for a from 1 upto b
                                   if (and (= (* c c) (+ (* a a) (* b b)))
                                           (= p (+ a b c)))
                                   do (incf solutions)))
                 finally (return solutions))))

    ;; TODO: optimize
    (iter (for p from 120 to upper-bound)
          (finding p maximizing (count-solutions p)))))

(defun euler-042 (words-file)
  "Count the number of triangle words appearing in WORDS-FILE"
  (labels ((letter-value (char)
             (1+ (- (char-code (char-downcase char)) (char-code #\a))))

           (word-score (word)
             (reduce #'+ (map 'list #'letter-value word))))

    (let ((triangle-numbers (make-hash-table))
          (upper-bound (loop for word in (read-from-file words-file)
                             maximize (word-score word))))
      (loop for n from 1
        for tn = (* 1/2 n (1+ n))
        until (> tn upper-bound)
        do (setf (gethash tn triangle-numbers) t))
      
      (loop for word in (read-from-file words-file)
        count (gethash (word-score word) triangle-numbers)))))

(defun euler-045 (lower-bound)
  "Return the smallest number greater than LOWER-BOUND that is simultaneously a triangle, pentagonal, and hexagonal number"
  ;; no need to check triangle numbers since every hexagonal number is a
  ;; triangle number
  (loop for hn from (1+ (floor (inverse-hexagonal lower-bound)))
        for n = (hexagonal-n hn)
        for pn = (inverse-pentagonal n)
        until (and (integralp pn) (= n (pentagonal-n (floor pn))))
        finally (return n)))

(defun euler-052 (k)
  "Find the smallest number x such that 2x,...,kx all contain the same digits"
  (loop for i from 1
        for nums = (mapcar (lambda (n) (* n i)) (loop for i from 2 upto k collect i))
        for digits = (mapcar (lambda (d) (sort d #'<)) (mapcar #'digits-of nums))
        until (every (lambda (d) (equal (first digits) d)) (rest digits))
        finally (return i)))

(defun euler-053 (threshold upper)
  "Count the number of times nCr exceeds THRESHOLD for 1<=n<=UPPER"
  ;; TODO: obviously this can be hugely optimised
  (loop for n from 1 upto upper
        sum (loop for r from 0 upto n
                  count (> (choose n r) threshold))))

(defun euler-085 (target)
  "Find the area of the nxm rectangle which contains the closest number of rectangles to TARGET"
  ;; TODO: analytic solution?
  (flet ((rectangles (n m)
           (loop for l from 0 below m
                 sum (loop for k from 0 below n
                           sum (* (- m l) (- n k))))))

    ;; TODO: calculate tight bounds?
    (loop with delta* and n* and m*
          for n from 1 upto 100
          do (loop for m from 1 upto 100
               for r = (rectangles n m)
               for delta = (abs (- target r))
               if (not delta*)
               do (setf delta* delta
                        n* n
                        m* m)
               else
               if (< delta delta*)
               do (setf delta* delta
                        n* n
                        m* m))
          finally (return (* n* m*)))))

(defun euler-092 (upper)
  "How many starting numbers below UPPER have a number chain arriving at 89?"
  (flet ((number-chain (n)
           "Return T if n's number chain reaches 89"
           (loop for i = n
                 then (reduce #'+ (mapcar (lambda (d) (* d d)) 
                                          (digits-of i)))
                 until (or (= i 89) (= i 1))
                 finally (return (= i 89)))))

    ;; probably there is some way to optimise by not counting explicitly
    (loop for i from 2 below upper
          count (number-chain i))))

(defun euler-102 (filename)
  "Return the number of times a triangle from FILENAME contains the origin in its interior"
  (loop with origin = #(0 0 0)
        for line in (read-from-file filename)
        for (ax ay bx by cx cy) = (mapcar #'parse-integer (str:split "," line))
        for a = (vector ax ay 0)
        and b = (vector bx by 0)
        and c = (vector cx cy 0)
        count (triangle-contains-point-p a b c origin)))

(defun euler-112 (percentage)
  "Find the least number for which the proportion of bouncy numbers is exactly PERCENTAGE"
  (loop for i from 1
        for bouncies = 0 then (+ bouncies (if (bouncyp i) 1 0))
        until (= percentage (* 100 (/ bouncies i)))
        finally (return i)))

(defun solve (problem-no fn &rest args)
  (format t "~D: ~D~%" problem-no (apply fn args)))

(defun main ()
  ;(format t "01: ~D~%" (euler-001 1000))
  (time
    (progn
      (solve "001" #'euler-001 1000)
      (solve "002" #'euler-002 4e6)
      (solve "003" #'euler-003 600851475143)
      (solve "004" #'euler-004 3)
      (solve "005" #'euler-005 20)
      (solve "006" #'euler-006 100)
      (solve "007" #'euler-007 10001)
      (solve "008" #'euler-008 "inputs/008.txt" 13)
      (solve "009" #'euler-009 1000)
      (solve "010" #'euler-010 2000000)
      (solve "012" #'euler-012 500)
      (solve "013" #'euler-013 "inputs/013.txt" 10)
      (solve "014" #'euler-014 1000000)
      (solve "015" #'euler-015 20 20)
      (solve "016" #'euler-016 (expt 2 1000))
      (solve "020" #'euler-020 (factorial 100))
      (solve "021" #'euler-021 10000)
      (solve "022" #'euler-022 "inputs/022.txt")
      (solve "023" #'euler-023)
      (solve "025" #'euler-025 1000) 
      (solve "029" #'euler-029 100 100) 
      (solve "030" #'euler-030 5)
      (solve "031" #'euler-031 200 #(1 2 5 10 20 50 100 200))
      (solve "035" #'euler-035 1000000)
      (solve "036" #'euler-036 1000000 '(2 10))
      ; (solve "039" #'euler-039 1000)
      (solve "042" #'euler-042 "inputs/words.txt")
      (solve "045" #'euler-045 40755)
      (solve "048" #'euler-048 1000 10)
      (solve "052" #'euler-052 6)
      (solve "053" #'euler-053 1000000 100)
      (solve "085" #'euler-085 2000000)
      (solve "092" #'euler-092 10e6)
      (solve "102" #'euler-102 "inputs/102.txt")   
      (solve "112" #'euler-112 99))))
