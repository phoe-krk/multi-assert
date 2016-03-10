;; MULTI-ASSERT by Michal Herda
;; License: GNU GPLv3

;; MULTI-ASSERT - a simple tool.
;; MULTI-ASSERT takes a list of variables and a list of predicates - almost like an
;; usual ASSERT. Though, the list of predicates is extensible and is executed with
;; nested ASSERTs in order in which they appear on the original list.

(defmacro %multi-assert (vars predicates)
  (when predicates
    `(assert (progn (%multi-assert ,vars ,(cdr predicates))
		    ,(caar predicates))
	     ,vars ,(cadar predicates) ,@(cddar predicates))))
(defmacro multi-assert (vars predicates)
  `(%multi-assert ,vars ,(reverse predicates))) 

;; MAKE-ASSERT - a powerful tool
;; In DEFMASSERT you specify the name under which this multi-assert will be available,
;; a list of required lexical variables, a list of dependencies (other multi-asserts that
;; need to be executed before this one) and the proper MULTI-ASSERT assertions to be included.

(eval-when (:compile-toplevel)
  (defparameter *multi-assert-parts*
    (make-hash-table))
  (defun slurp (x y)
    (remove-duplicates (apply #'append x y)))
  (defun vars (x)
    (first (gethash x *multi-assert-parts*)))
  (defun deps (x)
    (second (gethash x *multi-assert-parts*)))
  (defun code (x)
    (cddr (gethash x *multi-assert-parts*)))
  (defun get-deps (key)
    (slurp (deps key) (mapcar #'get-deps (deps key))))
  (defun get-vars (dep)
    (slurp (vars dep) (mapcar #'get-vars (vars dep))))
  (defun get-code (dep)
    (slurp (code dep) (mapcar #'get-codes (vars dep))))
  (defun get-vars-deps (key)
    (slurp (get-vars key) (mapcar #'get-vars (get-deps key))))
  (defun get-codes (key)
    (remove-duplicates (append (apply #'append (mapcar #'get-codes (deps key)))
			       (get-code key) ())))
  (defun get-codes-list (key)
    (apply #'append (get-codes key))))

(defmacro defmassert (key code &rest rest)
  `(progn (setf (gethash ,key *multi-assert-parts*) ',code)
	  ,(when rest `(multi-assert-push ,@rest))))

(defmacro make-assert (&rest keywords) 
  `(multi-assert ,(apply #'append (mapcar #'get-vars-deps keywords))
     ,(apply #'append (mapcar #'get-codes-list keywords))))
     
;; Examples of use:

(let ((x 15)) ; customize the X value to your liking.
  (multi-assert (x)
    (((typep x 'integer)
      "X must be an integer.")
     ((/= x 25)
      "X must not equal 25.") 
     ((< x 50)
      "X must be less than 50.")
     ((> x 0)
      "X must be more than 0."))))

;; MAKE-ASSERT examples and usage

(defmassert :name
    ((name)
     ()
     (((typep name 'string-designator)
       "NAME must be a string designator.")
      ((not (string= name ""))
       "NAME cannot be empty."))))
(defmassert :coords
    ((coords)
     ()
     (((typep coords 'cons)
       "COORDS must be a dotted pair of integers." coords)
      ((typep (car coords) 'integer)
       "X must be an integer.") 
      ((typep (cdr coords) 'integer)
       "Y must be an integer.")
      ((>= (car coords) 0)
       "X must be greater than 0.")
      ((>= (cdr coords) 0)
       "Y must be greater than 0."))))
(defmassert :coord-bounds
    ((coords world)
     (:coords)
     (((< (car coords)
	  (array-dimension (world-map world) 0))
       "~A value of ~D is beyond this world's ~A size ~D."
       'x (car coords) 'x
       (array-dimension (world-map world) 0))
      ((< (cdr coords)
	  (array-dimension (world-map world) 1))
       "~A value of ~D is beyond this world's ~A size ~D."
       'y (cdr coords) 'y
       (array-dimension (world-map world) 1)))))
(defmassert :resize
    ((world up left down right)
     (:coord-bounds)
     (((plusp (+ up down (array-dimension (world-map world) 0)))
       "The resulting X size must be at least 1.")
      ((plusp (+ left right (array-dimension (world-map world) 1)))
       "The resulting Y size must be at least 1."))))
       
;; And then in a context where the proper variables (WORLD UP LEFT DOWN RIGHT COORDS)
;; have assigned values:
 
(make-assert :coord-bounds :resize)
