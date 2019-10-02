(defun listp+ (x)
  (and x (listp x)))

(defun choice (&rest options)
  (cons 'choice-of options))

(defun choice? (form)
  (and (listp form) (eql (car form) 'choice-of)))

(defun combinations (l &optional acc)
  (if (not l) (reverse acc)
    (combinations 
      (rest l) 
      (if (not acc) (loop for x in (first l) collect (list x))
                    (apply #'append (loop for former in acc
                          collect (loop for n in (first l)
                                        collect (cons n former))))))))

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

(defun grep-choices (form &optional acc)
  (if (not form) (reverse acc)
    (grep-choices (rest form)
       (cond ((choice? (first form))
                 (cons (rest (first form)) acc))
             ((listp+ (first form))
                 (append acc (grep-choices (first form))))
             (t  acc)))))

(defun must-have (condition)
  (let ((combos (combinations (grep-choices condition))))
    (reduce (lambda (a v)
              (let ((ans (eval (merge-choices condition v))))
                (if ans (cons v a) a)))
            combos
            :initial-value nil)))

(print (eval (merge-choices '(+ (choice-of 1) (choice-of 2) (choice-of 3)) '(4 5 6))))
(print (grep-choices '(= (+ (choice-of 1) (choice-of 2 3)))))
(print (combinations '((1 2 3) (4 5 6) (7 8 9))))

(print (let ((a (choice 1 2 3))
             (b (choice 4 5 6)))
         (must-have `(= (+ ,a ,b) 5))))

(print (let ((a (choice 2 3 4))
             (b (choice 8 1 1))
             (c (choice 8 1 9)))
         (must-have `(= (+ (* ,a ,b) ,c) 40))))
