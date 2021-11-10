
;Copyright 1998 Tom M. Mitchell.  This code may be freely distributed
;and used for any non-commericial purpose, as long as this copyright
;notice is retained.  The author assumes absolutely no responsibility
;for any harm caused by bugs in the code.

(setf *print-length* nil)
(setf *print-level* nil)

;;; General utility functions
;; use (print.entity instance) to see the definition of entity 

(defun put.value (attribute instance val)
  "assign a value to an attribute of an instance"
  (setf (get instance attribute) val))

(defun get.value (attribute instance)
  "retrieve the value of attribute of instance"
  (get instance attribute))

(defun print.entity (instance)
  "print the description of instance to the screen"
  (print (symbol-plist instance)))


(defun data-setup (datafile)
(setq *examples* nil)
(setq our-input-stream (open datafile :direction :input))
(setq *attributes* (read our-input-stream))
(setq *att-class* (car (read our-input-stream)))
(setq *att-values* (read our-input-stream))
   
(loop for d in (read our-input-stream) do
      (setf *examples* (cons (first d) *examples*))
      (loop for attribute in (append *attributes* (list *att-class*))
	     as value in (cdr d)
	     do (put.value attribute (first d) value)))
(put.value 'legal.values *att-class* *att-values*)
(setq *examples* (reverse *examples*))
(close our-input-stream))


;;; Top level ID3 Decision Tree learning algorithm
;
; Tree Representation: each non-terminal tree node is a list of the form 
;  (attribute (value1 subtree1)(value2 subtree2)...)
;  where subtree-n is either a non-terminal node, or a value signifying the 
;  target value associated with that terminal node

(defun id3 (examples target.attribute attributes)
;
  "TARGET.ATTRIBUTE is the attribute to be predicted by the learned tree.
   EXAMPLES are training examples.  ATTRIBUTES is a list of attributes (not
   including TARGET.ATTRIBUTE) that may be included in the learned decision 
   tree.
   Returns: a decision tree that predicts the TARGET.ATTRIBUTE over EXAMPLES"
  (let (firstvalue a partitions)
    (setq firstvalue (get.value target.attribute (first examples)))
;    (break "in id3")
    (cond 
     ;; if every example has the same target attribute value, return it as
     ;; a leaf node
     ((every #'(lambda(e)(eq firstvalue (get.value target.attribute e)))
             examples)*att-values*
      firstvalue)
     ;; if no attributes, return the most common target attribute value
     ((null attributes)
      (most.common.value target.attribute examples))
     ;; otherwise, pick the best attribute, partition training data, and call
     ;; ID3 recursively to grow subtrees below this node
     (t
      (setq partitions
            (loop for a in attributes collect (partition a examples)))
      (setq a (choose.best.partition target.attribute partitions))
      (cons (first a)
            (loop for branch in (cdr a) collect
                  (list (first branch)
                        (id3 (cdr branch) 
                             target.attribute 
                             (remove (first a) attributes)))))))))

                  
(defun partition (attribute instances)
  "returns a partion of INSTANCES according to their values for ATTRIBUTE. 
   Returns a list (attribute (value1 e11 e12 ...)(value2 e21 e22 ...)...)"
  (let (result vlist v)
    (loop for e in instances do
          (setq v (get.value attribute e))
          (if (setq vlist (assoc v result))
            (rplacd vlist (cons e (cdr vlist)))
            (setq result (cons (list v e) result))))
    (cons attribute result)))

(defun choose.best.partition (target.attribute partitions)
  "return the partition with the highest information gain.  
   PARTITIONS is of the form ((attribute1 (val1 e11 e12 ...)(val2 e21 e22 ...)...)
                              (attribute2 (...  ...........)(...  ...  )...)).
   Note for efficiency, we compute only the expected value of the entropy of the
   partition, because this is the only term in information gain that varies from
   one attribute to another"
  (let ((lowest.exp.entropy 9999) exp.entropy best.partition)
    (loop for att.partition in partitions do
          (when (< (setq exp.entropy 
                         (expected.entropy target.attribute (cdr att.partition)))
                   lowest.exp.entropy)
            (setq lowest.exp.entropy exp.entropy)
            (setq best.partition att.partition)))
    best.partition))

(defun expected.entropy (att partition)
  "returns the sum over possible values of att of the quantity
    number.of.instances.with.this.value x sample.entropy.of.this.partition"
  (loop for p in partition sum
        (* (length (cdr p))
           (loop for v in (get.value 'legal.values att) sum
                 (let ((vcount (loop for e in (cdr p) count 
                                     (eq v (get.value att e))))
                       proportion)
                   (setq proportion (/ vcount (length (cdr p))))
;;                   (format t "p: ~S, vcount: ~d, proportion: ~S~%"
;;                           p vcount proportion)
                   (* -1.0 proportion 
                      (if (zerop proportion) 0 (log proportion 2))))))))

(defun most.common.value (attribute instances)
;  (break "in most-common-value")
  (let ((length 0) longest)
    (loop for p in (cdr (partition attribute instances)) do
          (when (> (length p) length)
            (setq length (length p))
            (setq longest p)))
    (car longest)))

(defun entropy (p)
  (+ (* -1.0 p (log p 2))
     (* -1.0 (- 1 p) (log (- 1 p) 2))))
                   

(defun print.tree (tree &optional (depth 0))
  (tab depth)
  (format t "~A~%" (first tree))
  (loop for subtree in (cdr tree) do
        (tab (+ depth 1))
        (format t "= ~A" (first subtree))
        (if (atom (second subtree))
          (format t " => ~A~%" (second subtree))
          (progn (terpri)(print.tree (second subtree) (+ depth 5))))))
(defun tab (n)
  (loop for i from 1 to n do (format t " ")))

(defun compute-subset (i j instances)
; i and j are integers
; instances is a list of data instances
; returns a list of the i-th thru j-th elements of instances
; YOU MUST WRITE THIS FUNCTION
)

(defun classify (instance tree)
; instance is a data instance from *examples*
; tree is a decision tree produced by id3
; Return the class value predicted by the decision tree for instance
; YOU MUST WRITE THIS FUNCTION
  )

(defun testing (testcases tree)
; testcases is a list of data instances to be used as testcases
; tree is a decision tree  
; returns the number of instances in testcases that are classified correctly
; YOU MUST WRITE THIS FUNCTION  
)

(defun cross-validate (n examples size)
; n is an integer
; examples is a list of data instances (in our case, it will be *examples*)
; size is the number of instances in examples 
; size must be evenly divisible by n
; returns the percentage error from n-fold cross-validation on the instances in examples
; YOU MUST WRITE THIS FUNCTION
)
