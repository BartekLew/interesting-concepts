;; Some helpers

(defun listp+ (x)
  (and x (listp x)))

(defmacro test (form result test)
  `(let ((ans ,form))
     (if (not (,test ans ,result))
       (format t "TEST FAILED: ~A != ~A" ans ,result))))

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

(defun combinations (l &optional acc)
  (if (not l) (reverse acc)
    (combinations 
      (rest l) 
      (if (not acc) (loop for x in (first l) collect (list x))
                    (apply #'append (loop for former in acc
                          collect (loop for n in (first l)
                                        collect (cons n former))))))))


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
  (let ((combos (combinations (grep-choices condition))))
    (reduce (lambda (a v)
              (let ((ans (eval (merge-choices condition v))))
                (if ans (cons v a) a)))
            combos
            :initial-value nil)))


;; Some tests…

(test (eval (merge-choices '(+ (choice-of 1) (choice-of 2) (choice-of 3)) '(4 5 6)))
      15 eql)

(test (grep-choices '(= (+ (choice-of 1) (choice-of 2 3))))
     '((2 3) (1)) equal)

(test (combinations '((1 2 3) (4 5 6) (7 8 9)))
      '((9 6 3) (8 6 3) (7 6 3) (9 5 3) (8 5 3) (7 5 3) (9 4 3) (8 4 3) (7 4 3)
        (9 6 2) (8 6 2) (7 6 2) (9 5 2) (8 5 2) (7 5 2) (9 4 2) (8 4 2) (7 4 2)
        (9 6 1) (8 6 1) (7 6 1) (9 5 1) (8 5 1) (7 5 1) (9 4 1) (8 4 1) (7 4 1))
      equal)

(test (let ((a (choice 1 2 3))
             (b (choice 4 5 6)))
         (must-have `(= (+ ,a ,b) 5)))
      '((1 4)) equal)

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
