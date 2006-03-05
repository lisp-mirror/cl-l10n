;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;; See the file LICENCE for licence information.
(in-package #:cl-l10n)

;;  Macros
;;;;;;;;;;;
(defmacro aif (test then &optional else)
  `(let ((it ,test))
      (if it ,then ,else)))

(defmacro acond (&rest options)
  (if (cdr options)
      `(aif ,(caar options)
            (progn ,@(cdar options))
            (acond ,@(cdr options)))
      `(aif ,(caar options)
            (progn ,@(cdar options)))))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var
       ,@body)))

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar #'(lambda (x) `(,x (gensym))) names)
    ,@body))


;; dont worry it's nothing like if*
(defmacro or* (&rest vals)
  "(or* (string= foo a b) (char= foo b)) == 
  (or (string= foo a) (string= foo b) (char= foo b))"
  `(or ,@(mappend #'(lambda (x)
                      (destructuring-bind (test val &rest args) x
                        (if (singlep args)
                            `((,test ,val ,@args))
                            (mapcar #'(lambda (y) 
                                        `(,test ,val ,y))
                                    args))))
                  vals)))


;; Functions
;;;;;;;;;;;;;;
(defun singlep (list)
  (and (consp list) 
       (not (cdr list))))

(defun last1 (list)
  (car (last list)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (x args)
      (princ x s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun mappend (fn &rest lists)
  (apply #'append (apply #'mapcar fn lists)))

(defun required-arg (name)
  (error "~A is a required argument" name))

(defvar *whitespace* (list #\Space #\Tab))

(defun trim (string &optional (bag *whitespace*))
  (string-trim bag string))

(defun group (list n)
  (assert (> n 0))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if list (rec list nil) nil)))

(defun winner (test get seq)
  (if (null seq)
      nil
      (let* ((val (elt seq 0))
             (res (funcall get val)))
        (dolist (x (subseq seq 1) (values val res))
          (let ((call (funcall get x)))
            (when (funcall test call res)
              (setf res call
                    val x)))))))

(defun compose (&rest fns)
  (if fns
      (let ((last-fn (last1 fns))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall
                    fns
                    :from-end t 
                    :initial-value (apply last-fn args))))
      #'identity))

(defun float-part (float)
  (if (zerop float)
      ""
      (multiple-value-call 'extract-float-part (flonum-to-digits float))))

(defun extract-float-part (dp-pos aft)
  (let ((length (length aft)))
    (if (> dp-pos length)
        ""
        (with-output-to-string (x)
          (cond ((minusp dp-pos)
                 (dotimes (z (abs dp-pos))
                   (princ 0 x))
                 (princ aft x))
                (t (princ (subseq aft dp-pos)
                          x)))))))

;; From sbcl sources (src/code/print.lisp)
(defconstant single-float-min-e
  (nth-value 1 (decode-float least-positive-single-float)))
(defconstant double-float-min-e
  (nth-value 1 (decode-float least-positive-double-float)))

(defun flonum-to-digits (v)
  (let ((print-base 10)                 ; B
        (float-radix 2)                 ; b
        (float-digits (float-digits v)) ; p
        (digit-characters "0123456789")
        (min-e
         (etypecase v
           (single-float single-float-min-e)
           (double-float double-float-min-e))))
    (multiple-value-bind (f e)
        (integer-decode-float v)
      (let ((high-ok (evenp f))
            (low-ok (evenp f))
            (result (make-array 50 :element-type 'base-char
                                :fill-pointer 0 :adjustable t)))
        (labels ((scale (r s m+ m-)
                   (do ((k 0 (1+ k))
                        (s s (* s print-base)))
                       ((not (or (> (+ r m+) s)
                                 (and high-ok (= (+ r m+) s))))
                        (do ((k k (1- k))
                             (r r (* r print-base))
                             (m+ m+ (* m+ print-base))
                             (m- m- (* m- print-base)))
                            ((not (or (< (* (+ r m+) print-base) s)
                                      (and (not high-ok) (= (* (+ r m+) print-base) s))))
                             (values k (generate r s m+ m-)))))))
                 (generate (r s m+ m-)
                   (let (d tc1 tc2)
                     (tagbody
                      loop
                       (setf (values d r) (truncate (* r print-base) s))
                       (setf m+ (* m+ print-base))
                       (setf m- (* m- print-base))
                       (setf tc1 (or (< r m-) (and low-ok (= r m-))))
                       (setf tc2 (or (> (+ r m+) s)
                                     (and high-ok (= (+ r m+) s))))
                       (when (or tc1 tc2)
                         (go end))
                       (vector-push-extend (char digit-characters d) result)
                       (go loop)
                      end
                       (let ((d (cond
                                  ((and (not tc1) tc2) (1+ d))
                                  ((and tc1 (not tc2)) d)
                                  (t    ; (and tc1 tc2)
                                   (if (< (* r 2) s) d (1+ d))))))
                         (vector-push-extend (char digit-characters d) result)
                         (return-from generate result))))))
          (if (>= e 0)
              (if (/= f (expt float-radix (1- float-digits)))
                  (let ((be (expt float-radix e)))
                    (scale (* f be 2) 2 be be))
                  (let* ((be (expt float-radix e))
                         (be1 (* be float-radix)))
                    (scale (* f be1 2) (* float-radix 2) be1 be)))
              (if (or (= e min-e) (/= f (expt float-radix (1- float-digits))))
                  (scale (* f 2) (* (expt float-radix (- e)) 2) 1 1)
                  (scale (* f float-radix 2)
                         (* (expt float-radix (- 1 e)) 2) float-radix 1))))))))
;; EOF