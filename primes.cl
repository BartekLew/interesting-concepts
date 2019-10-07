;; Some helpers

(defun listp+ (x)
  (and x (listp x)))

(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~A != ~A" ans ,result))))


(defmacro f (args &body body)
  `(labels ((self ,args ,@body))
     (lambda (&rest args)
       (apply #'self args))))

(test (apply (f (x)
                (if (> x 1) (* x (self (- x 1)))
                  1))
             '(5))
      120 eql)

(defun repeat (count val)
  (loop for i from 1 to count
        collect val))

;; In this code I achieve testing if number is prime
;; using non-determinitstic programming.

;; First I define way to say that variable can have
;; one of many values, which one is right, will be
;; figured out later…

(defun choice (&rest options)
  (cons 'choice-of options))

(defun choice? (form)
  (and (listp form) (eql (car form) 'choice-of)))


;; When testing expression consisting of many
;; variables with ambigous values, I have to
;; compute all possible combinations…

;; @ gets 2 lists. List of combinations and list
;; of indexes. Returns combination.

(defun @ (l idx)
  (cond ((numberp idx) (nth idx l))
        ((listp idx) (loop for i in idx
                           for x in l
                           collect (nth i x)))
        (t (error 'type-error))))

(test (@ '((1 2 3) (4 5 6)) '(0 0))
      '(1 4) equal)


;; @+ gets list of combinations and list of indexes.
;; Returns next possible combination of indexes or nil

(defun @+ (l idx &optional acc)
  (if (or (not idx) (not l)) (return-from @+ nil))

  (let ((nl (first l))
        (ni (+ 1 (first idx))))
    (if (< ni (length nl))
      (append acc (cons ni (rest idx)))
      (@+ (rest l) (rest idx)
          (append acc (list 0))))))

(test (let ((ls '((1 2 3) (4 5 6)))
             (idx '(0 0)))
         (loop for x = (@+ ls idx)
               while x
               collect (setf idx x)))
      '((1 0) (2 0) (0 1) (1 1) (2 1) (0 2) (1 2) (2 2))
      equal)


;; select takes list of combinations and function.
;; Function is applied to each combination.
;; Return list of all non-nil results.

(defun select (combos fun)
  (let ((idx (repeat (length combos) 0)))
    (apply (f (idx &optional acc)
              (if (not idx) (return-from self acc))

              (let ((ans (apply fun (@ combos idx))))
                (self (@+ combos idx)
                      (if ans (append acc (list (@ combos idx)))
                              acc))))
           (list idx))))

(test (select '((1 2 3) (3 4 5) (6 7 8))
               (f (a b c) (> (+ a b c) 12)))
    '((3 4 6) (2 5 6) (3 5 6) (3 3 7) (2 4 7) (3 4 7)
      (1 5 7) (2 5 7) (3 5 7) (2 3 8) (3 3 8) (1 4 8)
      (2 4 8) (3 4 8) (1 5 8) (2 5 8) (3 5 8))
    equal)

;; For each set of possible values I want to put them back
;; to original form

(defun merge-choices (form choice &optional acc)
  (cond ((not form) (values (reverse acc) choice))
        ((choice? (first form))
           (merge-choices (rest form)
                          (rest choice) 
                          (cons (first choice) acc)))
        ((listp+ (first form))
           (multiple-value-bind (match rest)
             (merge-choices (first form) choice)
             (merge-choices (rest form) rest (cons match acc))))
        (t (merge-choices (rest form)
                          choice
                          (cons (first form) acc)))))

;; I will also need to find all ambiguities in form…

(defun grep-choices (form &optional acc)
  (if (not form) (reverse acc)
    (grep-choices (rest form)
       (cond ((choice? (first form))
                 (cons (rest (first form)) acc))
             ((listp+ (first form))
                 (append acc (grep-choices (first form))))
             (t  acc)))))


;; Finally I can define function that chooses only
;; those possible values that meet condition

(defun must-have (condition)
  (select (grep-choices condition)
          (f (&rest x) (eval (merge-choices condition x)))))


;; Some tests…

(test (eval (merge-choices '(+ (choice-of 1) (choice-of 2) (choice-of 3)) '(4 5 6)))
      15 eql)

(test (grep-choices '(= (+ (choice-of 1) (choice-of 2 3))))
     '((2 3) (1)) equal)

(test (let ((a (choice 1 2 3))
             (b (choice 4 5 6)))
         (must-have `(= (+ ,a ,b) 5)))
      '((4 1)) equal)

(test (let ((a (choice 2 3 4))
             (b (choice 8 1 1))
             (c (choice 8 1 9)))
         (must-have `(= (+ (* ,a ,b) ,c) 40)))
      '((8 4 8)) equal)


;; Finally I define function prime? predicate.

(defun prime? (n)
  (let ((x (apply #'choice
                  (loop for i from 2 to (- n 1)
                        collect i))))
    (let ((divisors (must-have `(eql (mod ,n ,x) 0))))
      (if divisors
        (format t "~%~A is not prime, it has divisors: ~{~A~}"
                n divisors))
      (eql divisors nil))))

;; et voila

(print (prime? 5))  ; T
(print (prime? 6))  ; NIL (2 and 3)
(print (prime? 73)) ; T
(print (prime? 111)); NIL (3 and 37)
(print (prime? 199)); T

(defun coins (sum)
  (let ((coin (choice 0.01 0.02 0.05 0.1 0.2 0.5 1 2 5)))
    (loop for count from 1
          do (let ((combos (must-have `(= ,sum (+ ,@(repeat count coin))))))
               (if combos (return-from coins combos))))))

(print (coins 1.12))
