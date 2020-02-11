
(import [collections.abc [Iterable]])

;; (import
;;   [hy [HyKeyword]]
;;   [hy.contrib.hy-repr [hy-repr]])

(require [hy.contrib.loop [loop]])
(require [hy.contrib.walk [let]])

;; (import [cons [cons :as  cons/py car :as car/py cdr :as cdr/py]]) ;;;can not apply on lisp
;; (import [cons.core [ConsPair MaybeCons ConsNull]])


;(eval-and-compile
(import  functools)


(defclass Nil/cl [] 
  (defn --init-- [self]
    (setv
      self.car self
      self.cdr self)))
(setv nil/cl (Nil/cl))
(defn null/cl? [x]
  (cond
    [(instance? Nil/cl x) True]
    [(= [] x) True]
    [(= (,) x) True]
    [(= '() x) True]
    [True False]))

;;; take ConsPair from ohttps://github.com/algernon/adderall/blob/master/adderall/internal.hy
(defclass ConsPair [Iterable]
  "Construct a cons list.

A Python `list` is returned when the cdr is a `list` or `None`; otherwise, a
`ConsPair` is returned.

The arguments to `ConsPair` can be a car & cdr pair, or a sequence of objects to
be nested in `cons`es, e.g.

    (ConsPair car-1 car-2 car-3 cdr) == (ConsPair car-1 (cons car-2 (cons car-3 cdr)))
"
  (defn --new-- [cls &rest parts]
    (if (> (len parts) 2)
        (reduce (fn [x y] (ConsPair y x))
                (reversed parts))
        ;; Handle basic car, cdr case.
        (do (setv car-part (-none-to-empty-or-list
                             (first parts)))
            (setv cdr-part (-none-to-empty-or-list
                             (if
                               (and (coll? parts)(> (len parts) 1))
                               (last parts)
                               ;;None
                               nil/cl
                               )))
            ;;(print "c d o" car-part cdr-part cls parts)
            (cond
              [(instance? Nil/cl cdr-part)
               `(~car-part)]
              [(instance? hy.models.HyExpression cdr-part)
               `(~car-part ~@cdr-part)]
              [(tuple? cdr-part)
               (tuple (+ [car-part] (list cdr-part)))]
              [(list? cdr-part)
                    ;; Try to preserve the exact type of list
                    ;; (e.g. in case it's actually a HyList).
               (+ ((type cdr-part) [car-part]) cdr-part)]
              [True
                    (do
                      (setv instance (.--new-- (super ConsPair cls) cls))
                      (setv instance.car car-part)
                      (setv instance.cdr cdr-part)
                      instance)]))))
  (defn --hash-- [self]
    (hash [self.car, self.cdr]))
  (defn --eq-- [self other]
    (and (= (type self) (type other))
         (= self.car other.car)
         (= self.cdr other.cdr)))
  (defn --iter-- [self]
    (yield self.car)
    (if (coll? self.cdr) ;;(list? self.cdr)
        (for [x self.cdr] (yield x))
        (raise StopIteration))
    )
  (defn --repr-- [self]
    (.format "({} . {})" self.car self.cdr)))

(defn -none-to-empty-or-list [x]
  (cond
    ;;[(none? x) (list)]
    [(tuple? x) x]
    [(and (coll? x)
          (not (cons? x))
          (not (list? x)))
     (list x)]
    [True x]))

;; A synonym for ConsPair
(setv cons ConsPair)

;; (defun car (ls)
;;   (first ls))

(defn car [z]
  (or (getattr z "car" None)
      (-none-to-empty-or-list (first z))))

;; (defun cdr (ls)
;;   (cut ls 1))

(defn cdr [z]
  (or (getattr z "cdr" None)
      (cond
        [(instance? range z) (cut z 1)]
        [(iterator? z) (rest z)]
        [True ;;(or (tuple? z) (list? z))
             ((type z) (list (rest z)))]
        ;;[True (rest z)]
      ;;  ;; Try to preserve the exact type of list
      ;;  ;; (e.g. in case it's actually a HyList).
        ;;((type z) (list (rest z)))
        )))


(defn cons? [a]
  (cond [(null/cl? a) False]
        [(instance? ConsPair a) True]
        ;;[(coll? a) (not (empty? a)) True]
        [(or (list? a) (tuple? a) ) (not (empty? a)) True]
        [True False])
  ;; (if (or (and
  ;;           (list? a)
  ;;           (not (empty? a)))
  ;;         (instance? ConsPair a))
  ;;     True
  ;;     False))
  )

;; (defn cons? [x] (isinstance x ConsPair))
;; (defn consp [x] (cons? x))
(defn consp [el]  (cons? el))
  

(defn atom/cl? [x] (not (cons? x)))


;;(eval-and-compile
  ;; renamed functions
;;   (defmacro! setf (&rest args)
;;     ;; Beware of humongous stdout(in repl)!!
;;     `(do
;;        (setv ~@(get args (slice 0 (- (len args) 2))))       
;;        (setv ~@(get args (slice -2 None)))
;;        ~(get args -2)))
;; (+ 1 1)
(defmacro typep [obj objtype]
  `(is (type ~obj) ~objtype))

;;   ;; todo: &optional cannnot accept () form. (now only stupid [])
;;   (defmacro defun (name lambda-list doc &rest body)
;;     (setv lambda-list (lfor el lambda-list 
;;                             (if (is (type el) HyExpression)
;;                                 (list el)
;;                                 el))) 
;;     (if (not (typep doc str))
;;         `(defn ~name ~(list lambda-list) ~@(cons doc (HyExpression body)))
;;         `(defn ~name ~(list lambda-list) doc ~@body)))
(defn eq [x y]      (is x y))
(defn equals [x y]  (= x y))

  ;; ;; numerical functions
  ;; (defun mod (n m)
  ;;   (% n m))
  
  ;; (defun zerop (n)
  ;;   (= n 0))
  
  ;; (defun plusp (n)
  ;;   (> n 0))
  
  ;; (defun minusp (n)
  ;;   (< n 0))

  ;; (defun oddp (n)
  ;;   (zerop (mod n 2)))

  ;; (defun evenp (n)
  ;;   (not (oddp n)))

  ;; (defun divisible (n m)
  ;;   (zerop (mod n m)))
(defmacro incf [n  &optional [delta 1]]
  `(setv ~n (+ ~n ~delta)))

(defmacro decf [n &optional [delta 1]]
  `(setv ~n (- ~n ~delta)))

(defmacro 1+ [n]
  `(+ ~n 1))

(defmacro 1- [n]
  `(- ~n 1))
  
  ;; ;; list functions
  ;; ;; ------------------- DO NOT SET nil!!-----------------------------
  ;; (setf nil (HyExpression ()))
  ;; ;; ------------------- DO NOT SET nil!!-----------------------------

;; (defn null     [ls] (isinstance ls ConsNull))
;; (defn null/cl? [ls] (null ls))

  ;; (defun lst (&rest args)
  ;;   (HyExpression args))

  ;; (defun length (list)
  ;;   (len list))

  ;; (defun emptyp (ls)
  ;;   (zerop (length ls)))

(defn caar [ls]  (-> ls car car))
(defn cddr [ls]  (-> ls cdr cdr))
(defn cadr [ls]  (-> ls cdr car))
(defn cdar [ls]  (-> ls car cdr))

(defn apply/cl [f ls]  (f #*ls))
  
  ;; (defmacro push (el ls)
  ;;   `(setf ~ls (cons ~el ~ls)))

  ;; (defun nreverse (ls)
  ;;   (.reverse ls)
  ;;   ls)
(defn nconc [x y]
  (cond
    [(list? x) (do (.extend x y) x)]
    [True (cons x y)] ;;not correct dealing cdr pointer 
     ))

(defn append/cl [x y]  (if (empty? x) y  (nconc (car x) (append/cl (cdr x) y))))

(defn mapcan [func ls]
  (if (empty? ls)
      ls
      (append/cl
        (func (car ls))
        (mapcan func (cdr ls)))))
;;   ;; macros
(defmacro progn [&rest body]
  `(do ~@body))

;; (eval-and-compile
(defn mapcar [func &rest seqs]  ((type (car seqs))  (apply/cl (functools.partial map func) seqs))   )

;;   (defun group (src n)
;;     (HyExpression (apply zip (* [(iter src)] n)))))

;; (eval-and-compile
  
(defmacro lambda [lambda-list &rest body]
  `(fn ~(list lambda-list) ~@body))

(defmacro let/cl [var-pairs &rest body]
  (setv var-names (list (map first  var-pairs))
        var-vals  (list (map second var-pairs)))
  `(let [ ~@(+ #*(lfor (, x y) (zip var-names var-vals) [x y]))]
     ~@body
     ))
(defmacro let* [varval &rest body]
  (if (<= (len varval) 1)
      `(let/cl ~varval ~@body)
      `(let/cl (~(first varval))
         (let* ~(cut varval 1)
           ~@body))))

;;   (defmacro! prog1 (&rest body)
;;     `(let ((~g!sexp-1 ~(car body)))
;;           (progn
;;             ~@(cdr body)
;;             ~g!sexp-1)))

;;   (defmacro when (condition &rest body)
;;     `(if ~condition
;;          (progn
;;            ~@body)))

;;   (defmacro unless (condition &rest body)
;;     `(if (not ~condition)
;;          (progn
;;            ~@body)))  
  
;;   (defun pushr (ls el)
;;     (.append ls el))
  
;;   (defun pushl (ls el)
;;     (.insert ls 0 el))
  
;;   )

;; (eval-and-compile
;;   (defun flatten-1 (ls)
;;     (let ((acc ()))
;;          (for [el ls]
;;            (if (consp el)
;;                (nconc acc el)
;;                (.append acc el)))
;;          acc))


(defmacro! if/cl [o!c x &optional y]
  `(if (null/cl? ~o!c) ~y (if ~o!c ~x ~y)))

 (defmacro cond/cl [&rest branches]
  (loop
    ((ls branches)
       (cont (lambda (x) x)))    
      (if ls
          (recur (cdr ls) (lambda (x) (cont `(if ~(caar ls)
                                                 (progn ~@(cdar ls)) 
                                                 ~x))))
          (cont None))))
 

;;   (defmacro! case (exp &rest branches)
;;     `(let ((~g!val ~exp))
;;           (cond/cl ~@(list (map (lambda (br)
;;                                   (if (= (car br) 'otherwise)
;;                                       `(True ~@(cdr br))
;;                                       `((eq ~g!val ~(car br)) ~@(cdr br))))
;;                                 branches)))))

(defn subseq [seq start &optional end] (cut seq start end))


(defn destruc [pat seq n]
  (let [nil ((type pat) (list))]
    (if (null/cl? pat)
        nil
        (let [res (cond
                    [(atom/cl? pat) pat]
                    ;[(eq (car pat) '&rest) (cadr pat)]
                    [True nil])]
          (if/cl res
                 `((~res (subseq ~seq 0 ~n)))
                 (let
                   [p (car pat)
                    rec (destruc (cdr pat) seq (inc n))]
                   (if (atom/cl? p)
                       (cons
                         `(~p (get ~seq ~n))
                         rec)
                       (let [var (gensym)]
                         (cons (cons `(~var (get ~seq ~n))
                                  (destruc p var 0))
                               rec)))))))))

(defn dbind-ex [binds body]
  (if (null/cl? binds)
      `(do ~@body)
      `(let/cl
          ~(mapcar (fn [b]
                   (if (cons? (car b))
                       (car b)
                       b))
                 binds)
         ~(dbind-ex (mapcan (fn [b]
                             (if (cons? (car b))
                                 `(~(cdr b))
                                 '()))
                           binds)
                   body))))


(defmacro! dbind [pat seq &rest body]
   (if (instance? hy.models.HyExpression seq)
       (+ '(let) `([~g!seq (quote ~seq)])
          `( ~(dbind-ex (destruc pat g!seq 0) body))  )
       (+ '(let) `([~g!seq ~seq])
         `( ~(dbind-ex (destruc pat g!seq 0) body))
         )))


(defn values [&rest returns]   (tuple returns))

;;   ;; multiple-value-bind
;;   (defmacro mvb (var-list expr &rest body)
;;     `(dbind ~var-list ~expr ~@body))
(defmacro multiple-value-bind [var-list expr &rest body]
   (setv n1 (len var-list) n2 (len expr))
   `(do (setv
         ~@(mapcan
             (fn [k]
               (if (< k n2)
               [(get var-list k) (get expr k)]
               [(get var-list k) None] ))
             (list (range n1))))
       ~@body
       ))


;;   ;; errors
;;   (defmacro! ignore-errors (&rest body)
;;     `(try
;;        ~@body
;;        (except [~g!err Exception]
;;          nil)))

;;   (defmacro! unwind-protect (protected &rest body)
;;     `(try
;;        ~protected
;;        (except [~g!err Exception]
;;          ~@body
;;          (raise ~g!err))))

;;   ;; sharp macros
;;   (defmacro! pr (o!arg)
;;     `(do
;;        (print ~o!arg)
;;        ~o!arg))

;;   (deftag p [code]
;;     "debug print"
;;     `(pr ~code))

;;   (deftag r [regex]
;;     "regexp"
;;     `(do
;;        (import re)
;;        (re.compile ~regex)))

;;   (deftag g [path]
;;     "glob"
;;     `(do
;;        (import glob)
;;        (glob.glob ~path)))    
  
  
;;   (import os fnmatch)
  
;;   (defun path-genr (fname dir)
;;     (for [tp (os.walk dir)]       
;;       (for [f (get tp 2)]
;;         (if (fnmatch.fnmatch f fname)
;;             (yield (os.path.join (get tp 0) f))))))

;;   (deftag f [dir-fname]
;;     "find file name"
;;     `(path-genr ~(get dir-fname 1) ~(get dir-fname 0)))
  
;;   (deftag sh [command]
;;     `(do
;;        (import subprocess)
;;        (setf proc (subprocess.Popen ~command
;;                                     :shell True
;;                                     :stdin subprocess.PIPE
;;                                     :stdout subprocess.PIPE
;;                                     :stderr subprocess.PIPE)
;;              (, stdout-data stderr-data) (proc.communicate))
;;        (print (.decode  stderr-data "ascii"))
;;        (.decode stdout-data "ascii")))
  
;;   (defun ts ()
;;     "timestamp"
;;     (do
;;       (import datetime)       
;;       (datetime.datetime.now)))
;;   )

;; ;; pipe utils
;; (eval-and-compile
;;   (defun nreplace-el (from to tree &optional guard)
;;     (loop
;;       ((tree tree)
;;        (guard guard))
;;       (for [i (range (len tree))]
;;         (setv el (get tree i))        
;;         (cond/cl ((consp el)  (if (= (get el 0) guard) 
;;                                   (continue)
;;                                   (recur el guard)))
;;                  ((and (is (type el) (type from)) (= el from)) (setf (get tree i) to)))))
;;     tree)

;;   (defmacro! => (&rest args)
;;     (let ((replaced (nreplace-el '_ g!it (cdr args) '=>))
;;           (cur `(let ((~g!it ~(get args 0)))
;;                      ~g!it)))
;;          (for [sexp (cdr args)]        
;;            (setf cur (if (in g!it (flatten [sexp]))                    
;;                          `(let ((~g!it ~cur))
;;                                ~sexp)
;;                          (if (consp sexp)
;;                              (HyExpression (+ [(get sexp 0)] [cur] (cdr sexp)))
;;                              (+ (HyExpression [sexp]) [cur])))))
;;          cur))
  
;;   (defmacro first_ (ls)
;;     `(get ~ls 0))
  
;;   (defmacro last_ (ls)
;;     `(get ~ls -1))
  
;;   (defmacro spl/ (str)
;;     `(.split str "/"))
  
;;   (defmacro spl/t (str)
;;     `(.split str "/t"))
  
;;   (defmacro splc (str)
;;     `(.split str ","))
  
;;   (defmacro getext (str)
;;     `(=> ~str spl/ last_))
  
;;   (defmacro getid (str)
;;     `(=> ~str spl/ first_))
  
  
;;   )


;; (eval-and-compile   
;;   (defun slurp (path &optional (encoding "latin-1"))
;;     (.read (open path 'r :encoding encoding)))

;;   (defun slurpls (path &optional (delim None) (encoding "latin-1"))
;;     (with (fr (open path 'r :encoding encoding))
;;       (if delim
;;           (lfor
;;             line fr
;;             (if delim
;;                (.split (.strip line) delim)
;;                (.strip line)))
;;           (.readlines fr))))

;;   (defun barf (cont path)
;;     (with (fw (open path 'w))
;;       (.write fw cont)))
  
;;   (defun barfls (ls path &optional (delim None))    
;;     (with (fw (open path 'w))
;;       (if delim
;;           (for [lst ls]
;;             (if delim
;;                 (=> lst 
;;                     (map str _)
;;                     (.join delim _)
;;                     (print :file fw))
;;                 (print lst :file fw))))))
  
;;   )

;; ;; debugging utils
;; (eval-and-compile
;;   (deftag bp []
;;     ;; breakpoint
;;     `(do (import ptpdb) (ptpdb.set_trace)))
  
;;   (import [io [StringIO]]
;;           traceback
;;           sys
;;           code
;;           [IPython.terminal.embed [InteractiveShellEmbed :as ise]]
;;           [IPython.lib.pretty [pretty]])  
  
;;   (defclass CallStackViewer [object]
;;     (defun __init__ (self tb)
;;       (setf self.tb tb
;;             self.frames []
;;             self.injected-symbols (set ["dv" "get_locs"]))
;;       (while tb.tb-next
;;         (setf tb tb.tb-next)
;;         (.append self.frames tb.tb-frame))
;;       (setf self.last-frame (get self.frames -1)))
    
;;     (defun get-locs (self &optional (n 5))
;;       (setf locs [])
;;       (for [frame (get self.frames (slice (- n) None))]
;;         (setf code frame.f-code
;;               args (dict-comp
;;                      arg (get frame.f-locals arg)
;;                      (arg (get code.co-varnames (slice None code.co-argcount))))
;;               loc-vars (dict-comp
;;                          k v
;;                          ((, k v) (.items frame.f-locals))
;;                          (if (not-in k self.injected-symbols))))
;;         (.append locs (, code.co-name args loc-vars)))
;;       locs))
  
;;   (defun debug (f)
;;     ;; postmortem (in failed call stack)
;;     ;; 1. Don't use in hy-mode repl
;;     ;; 2. your script will fail for circular importing in IPython/core/completer.py. so run a script with $ hy -c "(import script)(script.main)"
;;     (with-decorator (functools.wraps f)
;;       (defun wrapper (&rest args &kwargs kwargs)
;;         (try
;;           (f #*args #**kwargs)
;;           (except [e Exception]
;;             (print "Debug mode activated")
;;             (setf (, type value tb) (sys.exc-info)
;;                   buf (StringIO))
;;             (traceback.print-exc :file buf)
;;             (setf stb (pr (.getvalue buf))
;;                   dv (CallStackViewer tb)
;;                   frame dv.last-frame
;;                   ns frame.f-locals
;;                   (get ns 'dv) dv
;;                   (get ns 'get-locs) (lambda (&optional [n 5]) (dv.get-locs :n n)))
;;             (.mainloop (ise) :local-ns ns)))))
;;     wrapper)
  
;;   (deftag d [function-defininition-form]
;;     ;; Try this! Enjoy!
;;     ;; #d
;;     ;; (defun test ()
;;     ;;   (setf a 10)
;;     ;;   (/ 1 0))
;;     `(with-decorator debug
;;        ~function-defininition-form))
  
;;   (defmacro me (sexp)
;;     ;; use it with this emacs-lisp-command)
;;     ;; (defun pp-macroexpand ()
;;     ;;   (interactive "*")  
;;     ;;   (when (get-buffer "*Hy Macroexpand*")
;;     ;;     (kill-buffer "*Hy Macroexpand*"))
;;     ;;   (let ((beg nil)
;;     ;;          (end nil))
;;     ;;     (beginning-of-defun)
;;     ;;     (setf beg (point))
;;     ;;     (end-of-defun)
;;     ;;     (setf end (point))
;;     ;;     (let ((sexp (car (read-from-string
;;     ;;                        (buffer-substring beg end))))
;;     ;;            (buf (get-buffer-create "*Hy Macroexpand*")))
;;     ;;       (pp sexp buf)
;;     ;;       (display-buffer buf))))
;;     `(=> ~sexp
;;          macroexpand-1
;;          hy-repr       
;;          (.replace ";" "")
;;          (.replace "(." "(!dot")
;;          (get _ (slice 1 (len _)))       
;;          print))
;;   )

;; ;; nputils
;; (eval-and-compile
;;   (defun parse-indexing (sym)
;;     (if (not (in ":" (str sym))) 
;;         sym
;;         (progn
;;           (setf sym (str sym) 
;;                 splited (.split sym ":"))
;;           (list (map (lambda (el)
;;                        (if (or (not el) (= el "\ufdd0"))
;;                            None
;;                            (progn
;;                              (setf iel (ignore-errors (int el)))
;;                              (if iel
;;                                  iel
;;                                  (HySymbol el)))))
;;                      splited)))))
  
;;   (defun parse-str-indexing (str-i)
;;     (let ((splited (.split str-i ":")))
;;          (lfor
;;            i splited
;;            (get (hy.lex.tokenize i) 0))))
  
;;   (defmacro nget (ar &rest indices)
;;     `(get ~ar ~(tuple (lfor
;;                         i indices
;;                         (cond/cl
;;                           ((or (symbol? i) (keyword? i)) `(slice ~@(parse-indexing i)))
;;                           ((string? i) (parse-str-indexing i))
;;                           (True i))))))
  
;;   )



;;(defn assoc/cl [e dic] (if (in e dic) (get dic e) None))
(defn assoc/cl [e dic] (if (in e dic) (get dic e) nil/cl))
;; (assoc/cl 'x {'x 10 'y 20})

