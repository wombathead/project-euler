(defun problem1 (n)
  "Computes the sum of multiples of 3 and/or 5 less than n"
  (setf sum 0)
  (loop for i from 3 to n do
	(if (or (= (mod i 3) 0) (= (mod i 5) 0))
	  (incf sum i)))
  (return-from problem1 sum))

(defun problem2 (n)
  "Computes the sum of even Fibonacci numbers less than n"
  (setf a 1)
  (setf b 2)
  (setf sum 2)
  (loop while (<= (+ a b) n) do
		(setf c (+ a b))
		(if (= (mod c 2) 0)
		  (incf sum c))
		(setf a b)
		(setf b c))
  (return-from problem2 sum))

(defun get-factors (n)
  "Return a list of factors of n"
  (let ((factors '()) (root (+ 1 (isqrt n))))
	(loop for i from 2 to root do
		  (if (= 0 (mod n i))
			(if (/= i (/ n i))
			  (setf factors (append (list i (/ n i)) factors))
			  (setf factors (cons i factors)))))
	(return-from get-factors (remove-duplicates (sort factors #'>)))))

(defun is-prime (n)
  "Returns whether n is prime"
  ; first check whether n == 2 or n is even
  (if (= n 2)
	(return t))
  (if (= 0 (mod n 2))
	(return nil))

  (setf root (+ 1 (isqrt n)))
  (loop for i from 3 to root by 2 do
		(if (= 0 (mod n i))
		  (return-from is-prime nil)))

  (return-from is-prime t))

(defun problem3 (n)
  "Computes the largest prime factor of n"
  (setf factors (get-factors n))
  (setf max 0)
  (loop for f in factors do
		(if (and (is-prime f) (> f max))
		  (setf max f)))
  (return-from problem3 max))

(defun palindrome-p (n)
  (let ((str (write-to-string n)))
	(string= str (reverse str))))

(defun problem4 ()
  "Compute largest palindrome product of two 3-digit numbers"
  (let ((max 0))
	(loop for i from 100 to 999 do
		  (loop for j from 100 to 999 do
				(let ((prod (* i j)))
				  (if (and (palindrome-p prod) (> prod max))
					(setf max prod)))))
	(return-from problem4 max)))

(defun problem5 (n)
  "Compute smallest number divisible by all numbers 2..n"
  (let ((a 1))
	(loop for i from 2 to n do
		  (setf a (lcm a i)))
	(return-from problem5 a)))

(defun problem6 (n)
  "Difference between sum of squares and square of sum"
  (let ((nums (loop for i from 1 to n collect i)))
	(setf sum-squares (apply '+ (mapcar (lambda (x) (* x x)) nums)))
	(setf square-sum (expt (apply '+ nums) 2)))
  (- square-sum sum-squares))

(defun problem7 (n)
  "nth prime number"
  ())
