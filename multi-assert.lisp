;; MULTI-ASSERT by Michal Herda
;; License: GNU GPLv3

(defmacro multi-assert (vars predicates)
  `(%multi-assert ,vars ,(reverse predicates)))
(defmacro %multi-assert (vars predicates)
  (if predicates
      `(assert (progn (%multi-assert ,vars ,(cdr predicates))
		      ,(caar predicates))
	       ,vars ,(cadar predicates) ,@(cddadr predicates))))

;; Example usage, customize the X value to your liking.

(let ((x 15))
  (multi-assert (x)
    (((typep x 'integer)
      "X must be an integer.")
     ((/= x 25)
      "X must not equal 25.") 
     ((< x 50)
      "X must be less than 50.")
     ((> x 0)
      "X must be more than 0."))))
