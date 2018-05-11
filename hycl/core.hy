(import
  (hy (HyKeyword))
  (hy.contrib.hy-repr (hy-repr)))
(require
  (hy.contrib.loop (loop)))

(import
  re
  glob
  random
  fnmatch
  (functools :as ft))


(eval-and-compile
  (import (functools :as ft))
  ;; renamed functions
  (defmacro setf (&rest args)
    `(setv ~@args))

  (defmacro typep (obj objtype)
    `(is (type ~obj) ~objtype))

  (defmacro defun (name lambda-list doc &rest body)
    (if (not (typep doc str))
        `(defn ~name ~lambda-list ~@(cons doc body))
        `(defn ~name ~lambda-list doc ~@body)))

  (defun eq (x y)
    (is x y))

  (defun equals (x y)
    (= x y))

  ;; numerical functions
  (defun mod (n m)
    (% n m))
  
  (defun zerop (n)
    (= n 0))
  
  (defun plusp (n)
    (> n 0))
  
  (defun minusp (n)
    (< n 0))

  (defun oddp (n)
    (zerop (mod n 2)))

  (defun evenp (n)
    (not (oddp n)))

  (defun divisible (n m)
    (zerop (mod n m)))

  (defmacro incf (n &optional (delta 1))
    `(setf ~n (+ ~n ~delta)))

  (defmacro decf (n &optional (delta 1))
    `(setf ~n (- ~n ~delta)))

  ;; list functions
  ;; ------------------- DO NOT SET nil!!-----------------------------
  (setf nil (HyExpression ()))
  ;; ------------------- DO NOT SET nil!!-----------------------------

  (defun null (ls)
    (= nil ls))

  (setf HyCons (type '(1 . 2)))

  (defun lst (&rest args)
    (HyExpression args))

  (defun length (list)
    (len list))

  (defun emptyp (ls)
    (zerop (length ls)))

  (defun consp (el)
    (and (not (= el nil))
         (or (typep el HyExpression)
             (typep el HyCons))))

  (defun car (ls)
    (if (typep ls HyCons)
        (. ls car)
        (first ls)))
  
  (defun cdr (ls)
    (cut ls 1))
  
  (defun caar (ls)
    (-> ls car car))
  
  (defun cddr (ls)
    (-> ls cdr cdr))

  (defun cadr (ls)
    (-> ls cdr car))

  (defun cdar (ls)
    (-> ls car cdr))
  
  (defun apply (fn ls)
    (fn #*ls))  
  
  (defmacro push (el ls)
    `(setf ~ls (cons ~el ~ls)))

  (defun nreverse (ls)
    (.reverse ls)
    ls)

  (defun nconc (x y)
    (.extend x y)
    x)

  (defun last (ls)
    (get ls (dec (length ls))))

  (defun mapcan (func ls)
    (loop
      ((ls ls)
        (acc ()))
      (if ls
          (recur (cdr ls) (nconc acc (func (car ls))))
          (HyExpression acc))))
  
  (defun append (ls1 ls2)
    (+ ls1 ls2))
  ;; macros
  (defmacro progn (&rest body)
    `(do ~@body))  
  )

(eval-and-compile
  (defun mapcar (func &rest seqs)
    (HyExpression
      (apply (ft.partial map func) seqs)))
  
  (defun group (src n)
    (HyExpression (apply zip (* [(iter src)] n)))))

(eval-and-compile
  
  (defmacro lambda (lambda-list &rest body)
    `(fn ~lambda-list ~@body))
  
  (defmacro/g! let (var:val &rest body)
    `((lambda ~(mapcar car var:val) ~@body)
       ~@(mapcar cadr var:val)))

  (defmacro/g! let* (var:val &rest body)
    (loop
      ((ls (nreverse var:val))
        (acc body))
      (if ls
          (recur (cdr ls) `(let (~(car ls))
                                ~@(if (= acc body)
                                      body
                                      `(~acc))))
          acc)))  

  (defmacro/g! prog1 (&rest body)
    `(let ((~g!sexp-1 ~(car body)))
          (progn
            ~@(cdr body)
            ~g!sexp-1)))

  (defmacro when (condition &rest body)
    `(if ~condition
         (progn
           ~@body)))

  (defmacro unless (condition &rest body)
    `(if (not ~condition)
         (progn
           ~@body)))  
  
  (defun pushl (ls el)
    (.append ls el))

  (defun flatten-1 (ls)
    (let ((acc ()))
         (for (el ls)
           (if (consp el)
               (nconc acc el)
               (.append acc el)))
         acc))

  )

(eval-and-compile
  (defmacro cond/cl (&rest branches)
    (loop
      ((ls branches)
        (cont (lambda (x) x)))    
      (if ls
          (recur (cdr ls) (lambda (x) (cont `(if ~(caar ls)
                                                 (progn ~@(cdar ls)) 
                                                 ~x))))
          (cont None))))

  (defmacro/g! case (exp &rest branches)
    `(let ((~g!val ~exp))
          (cond/cl ~@(list (map (lambda (br)
                                  (if (= (car br) 'otherwise)
                                      `(True ~@(cdr br))
                                      `((eq ~g!val ~(car br)) ~@(cdr br))))
                                branches)))))

  (defun subseq (seq start end)   
    (case (type seq)
          (str (.join "" ))
          (list (list (islice seq start end)))
          (HySymbol (HySymbol (.join "" (islice seq start end))))
          (HyExpression (HyExpression (islice seq start end)))
          (HyKeyword (HyKeyword (.join "" (islice seq start end))))
          (otherwise (raise TypeError))))
  
  (defun destruc (pat seq n)
    (if (null pat)
        nil
        (let ((rest (cond/cl ((not (consp pat)) pat)
                             ((eq (car pat) '&rest) (cadr pat))
                             (True nil))))
             (if rest
                 `((~rest (subseq ~seq 0 ~n)))
                 (let ((p (car pat))
                          (rec (destruc (cdr pat) seq (+ n 1))))
                     (if (not (consp p)) 
                         (cons `(~p (get ~seq ~n))
                               rec)
                         (let ((var (gensym)))
                           (cons (cons `(~var (get ~seq ~n))
                                       (destruc p var 0))
                                 rec))))))))

  (defun dbind-ex (binds body)
    (if (null binds)
        `(progn ~@body)
        `(let ~(mapcar (lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                       binds)
              ~(dbind-ex (mapcan (lambda (b)
                                   (if (consp (car b))
                                       (cdr b)
                                       nil))
                                 binds)
                         body))))

  (defmacro/g! dbind (pat seq &rest body)
    `(let ((~g!seq ~seq))
          ~(dbind-ex (destruc pat g!seq 0) body)))


  (defmacro values (&rest returns)
    `(HyExpression (list ~returns)))

  ;; multiple-value-bind
  (defmacro mvb (var-list expr &rest body)
    `(dbind ~var-list ~expr ~@body))

  ;; errors
  (defmacro/g! ignore-errors (&rest body)
    `(try
       ~@body
       (except (~g!err Exception)
         nil)))

  (defmacro/g! unwind-protect (protected &rest body)
    `(try
       ~protected
       (except (~g!err Exception)
         ~@body
         (raise ~g!err))))

  ;; sharp macros
  (defmacro/g! pr (&rest args)
    `(let ((~g!once ~(car args)))
       (print ~g!once ~@(cdr  args))
       ~g!once))
  
  (defun pprint (expr)
    (hy-repr expr)) 

  (deftag p (code)
    "debug print"
    `(pr ~code))

  (deftag r (regex)
    "regexp"
    `(re.compile ~regex))

  (deftag g (path)
    "glob"
    `(glob.glob ~path))

  (defun path-genr (fname dir)
    (for (tp (os.walk dir))       
      (for (f (get tp 2))
        (if (fnmatch.fnmatch f fname)
            (yield (os.path.join (get tp 0) f))))))

  (deftag f (dir-fname)
    "find file name"
    `(path-genr ~(get dir-fname 1) ~(get dir-fname 0)))
  )


(eval-and-compile
  (defun nreplace-el (from to tree &optional guard)
    (loop
      ((tree tree)
       (guard guard))
      (for (i (range (len tree)))
        (setv el (get tree i))        
        (cond/cl ((consp el)  (if (= (get el 0) guard) 
                                  (continue)
                                  (recur el guard)))
                 ((and (is (type el) (type from)) (= el from)) (setf (get tree i) to)))))
    tree)

  (defmacro! => (&rest args)
    (let ((replaced (nreplace-el '_ g!it (cdr args) '=>))
           (cur `(let ((~g!it ~(get args 0)))
                   ~g!it)))
      (for (sexp (cdr args))        
        (setf cur (if (in g!it (flatten [sexp]))                    
                      `(let ((~g!it ~cur))
                         ~sexp)
                      (if (consp sexp)
                          (HyExpression (+ [(get sexp 0)] [cur] (cdr sexp)))
                          (+ (HyExpression [sexp]) [cur])))))
      cur)))
