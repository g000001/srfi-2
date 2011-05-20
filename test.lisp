(cl:in-package :srfi-2-internal)

(def-suite srfi-2)

(in-suite srfi-2)

(defmacro expect (a b)
  `(is (equal ,a ,b)))

(test and-let*
  (expect  (and-let* () 1) 1)
  (expect  (and-let* () 1 2) 2)
  (expect  (and-let* () ) T)

  (expect (let ((x nil)) (and-let* (x))) nil)
  (expect (let ((x 1)) (and-let* (x))) 1)
  (expect (and-let* ((x nil)) ) nil)
  (expect (and-let* ((x 1)) ) 1)
  ;; () = nil
  (expect (and-let* ( nil (x 1)))
          () )
  (expect (and-let* ( (nil) (x 1)) ) nil)
  (signals
      (error "An ill-formed binding in a syntactic form land* ")
    (and-let* (2 (x 1))) )
  (expect (and-let* ( (2) (x 1)) ) 1)
  (expect (and-let* ( (x 1) (2)) ) 2)
  (expect (let ((x nil)) (and-let* (x) x)) nil)
  (expect (let ((x "")) (and-let* (x) x)) "")
  (expect (let ((x "")) (and-let* (x)  )) "")
  (expect (let ((x 1)) (and-let* (x) (+ x 1))) 2)
  (expect (let ((x nil)) (and-let* (x) (+ x 1))) nil)
  (expect (let ((x 1)) (and-let* (((plusp x))) (+ x 1))) 2)
  (expect (let ((x 1)) (and-let* (((plusp x))) )) T)
  (expect (let ((x 0)) (and-let* (((plusp x))) (+ x 1))) nil)
  (expect (let ((x 1)) (and-let* (((plusp x)) (x (+ x 1))) (+ x 1)))  3)
  (signals
      (error "Execution of a form compiled with errors.")
    (let ((x 1)) (and-let* (((plusp x)) (x (+ x 1)) (x (+ x 1))) (+ x 1))) )
  (expect (let ((x 1)) (and-let* (x ((plusp x))) (+ x 1))) 2)
  (expect (let ((x 1)) (and-let* ( ((progn x)) ((plusp x))) (+ x 1))) 2)
  (expect (let ((x 0)) (and-let* (x ((plusp x))) (+ x 1))) nil)
  (expect (let ((x nil)) (and-let* (x ((plusp x))) (+ x 1))) nil)
  (expect (let ((x nil)) (and-let* ( ((progn x)) ((plusp x))) (+ x 1))) nil)

  (expect  (let ((x 1)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
  (expect  (let ((x 0)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
  (expect  (let ((x nil)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) nil)
  (expect  (let ((x 3)) (and-let* (x (y (- x 1)) ((plusp y))) (/ x y))) 3/2))
