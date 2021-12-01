;;;; TODO: make sure this works with numthy.lisp

(defun prime-p (n)
  (case n
    (1 nil)
    (2 t)
    (3 t)
    (t (loop for prime = t then (if (zerop (mod n i)) nil prime)
             for i from 2 upto (isqrt n)
             until (not prime)
             finally (return prime)))))

(defun pollard-rho (n &optional g)
  "Return a prime factor of N using polynomial G"
  (cond
    ((< n 2) nil)
    ((evenp n) 2)
    (t (let* ((g (if g g (lambda (x) (mod (+ (* x x) (random n)) n))))
              (result (loop for x = 2 then (funcall g x)
                            for y = 2 then (funcall g (funcall g y))
                            for d = 1 then (gcd (abs (- x y)) n)
                            while (= d 1)
                            finally (return d))))
         (if (= result n) n result)))))

(defun prime-factors (n)
  "Return the prime factorization of N as a list"
  (loop for m = n then (/ m k)
        for k = (pollard-rho m)
        until (null k)
        collect k))

(defun palindrome-p (n)
  (let ((str (write-to-string n)))
    (string= str (reverse str))))

(defun eratosthenes-sieve (n)
  "Return list of all primes below N"
  (loop with composites = (make-array (list n) :initial-element nil)
        for i = 2 then (loop for j from (1+ i) below n
                             unless (aref composites j)
                             minimize j)
        until (zerop i)
        do (loop for j from 1 upto (floor n i)
                 for idx = (* i j)
                 if (< idx n)
                 do (setf (aref composites idx) 1))
        collect i))

(defun euler-1 (n)
  "Sum of multiples of 3 or 5 less than N"
  (loop for i from 1 below n
        if (or (zerop (mod i 3)) (zerop (mod i 5)))
        sum i))

(defun euler-2 (n)
  "Sum of even Fibonacci numbers not exceeding N"
  (loop for a = 0 then b
        for b = 1 then c
        for c = (+ a b)
        when (evenp c)
        sum c
        until (> c n)))

(defun euler-3 (n)
  "Computes the largest prime factor of N"
  (reduce #'max (prime-factors n)))

(defun euler-4 (n)
  "Compute largest palindrome product of two N-digit numbers"
  (let ((smallest (expt 10 (1- n)))
        (largest (1- (expt 10 n))))
    (loop for i from smallest upto largest
          maximize (loop for j from smallest upto largest
                         for product = (* i j)
                         if (palindrome-p product)
                         maximize product))))

(defun euler-5 (n)
  "Compute smallest number divisible by all numbers 2..N"
  (loop for a = 1 then (lcm a i)
        for i from 2 upto n
        finally (return a)))

(defun euler-6 (n)
  "Difference between sum of squares and square of sum for the first N natural numbers"
  (let ((nums (loop for i from 1 to n collect i)))
    (abs (- (expt (reduce #'+ nums) 2)
            (reduce #'+ (mapcar (lambda (x) (* x x)) nums))))))

(defun euler-7 (n)
  "Return Nth prime number"
  (if (< n 2)
      2
      (loop for i from 3 by 2
            for is-prime = (prime-p i)
            for p = (if is-prime i p)    
            if is-prime
            count is-prime into n-primes
            until (>= (1+ n-primes) n)
            finally (return p))))

(defun euler-8 (n m)
  "Find the M adjacent digits in string N with the largest product"
  (loop with l = (length n) 
        for i from 0 upto (- l m)
        for window = (subseq n i (+ i m))
        maximize (reduce #'* (map 'list (lambda (c) (- (char-int c) (char-int #\0))) window))))

(defun euler-9 (n)
  "Find a Pythagorean triple whose sum is N"
  (loop for a from 1 upto n
        do (loop for b from (1+ a) upto n
                 do (loop for c from (1+ b) upto n
                          for a2 = (* a a) and b2 = (* b b) and c2 = (* c c)
                          do (if (and (= (+ a2 b2) c2)
                                      (= (+ a b c) n))
                                 (return-from euler-9 (* a b c)))))))

(defun euler-10 (n)
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

(defparameter *euler-8-input*
  "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450")

(defun main ()
  (format t "01: ~D~%" (euler-1 1000))
  (format t "02: ~D~%" (euler-2 4e6))
  (format t "03: ~D~%" (euler-3 600851475143))
  (format t "04: ~D~%" (euler-4 3))
  (format t "05: ~D~%" (euler-5 20))
  (format t "06: ~D~%" (euler-6 100))
  (format t "07: ~D~%" (euler-7 10001))
  (format t "08: ~D~%" (euler-8 *euler-8-input* 13))
  (format t "09: ~D~%" (euler-9 1000))
  (format t "10: ~D~%" (euler-10 2000000))
  )
