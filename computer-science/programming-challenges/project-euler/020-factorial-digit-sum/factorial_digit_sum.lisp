#|
  Common Lisp objects have types, while variables do not.
  https://lispcookbook.github.io/cl-cookbook/type.html
|#

(defun factorial (n)
  "Return the factorial of n."
  ; Return statements don't need to be explicit. The result of the last
  ; evaluated expression is returned.
  (if (= n 1)
      n
      (* n
	 (factorial (- n 1)))))

(defun sum_of_digits (n)
  "Return the sum of the digits in n."
  #|
    * https://lispcookbook.github.io/cl-cookbook/strings.html
    * https://lispcookbook.github.io/cl-cookbook/iteration.html
  |#
  (loop for c across (write-to-string n)
	sum (parse-integer (string c)) into total
	finally (return total)))

(defun project_euler_020 ()
  "Solve https://projecteuler.net/problem=20"
  (write (sum_of_digits (factorial 100))))



