(import [nose.tools [eq_  assert-equal assert-not-equal]])

(require [hy.contrib.walk [let]])

(import  [hycl.core [*]])
(require [hycl.core [*]])


(defn test-mapcan []
  (eq_
    (mapcan     (fn [x] [(+ x 10) "x"]) [1 2 3 4])
    [11 "x" 12 "x" 13 "x" 14 "x"])

  (eq_
    (mapcan     (fn [x] [(+ x 10) None]) [1 2 3 4])
    [11 None 12 None 13 None 14 None])
  )
  
(defn test-mapcar []
  (eq_
    (mapcar     (fn [x] [(+ x 10) "x"]) [1 2 3 4])
    [[11 "x"] [12 "x"] [13 "x"] [14 "x"]])

  (eq_
    (mapcar     (fn [x] [(+ x 10) None]) [1 2 3 4])
    [[11 None] [12 None] [13 None] [14 None ]])
  )

(defn test-let/cl []
  (eq_
    (let/cl ((x 1)
             (y 2))
      (setv y (+ x y))
      [x y])
    [1 3] )
  )


(defn test-cond/cl []
  (eq_
    (cond/cl
      ((= 1 2) "aa")
      ((= 2 2) "bb"))
    "bb")
  )

(defn test-multiple-value-bind []
  (eq_
    (multiple-value-bind
      (x y z u)
      (1 2 3)
      [x y z u])
    [1 2 3 None]
    )
  )

(defn test-dbind []
  (eq_
    (dbind
      (a (b c) d) (1 (2 3) 4)
      [a b c d])
    [1 2 3 4]
    )

  (eq_
    (dbind
      (a (b c) d) (1 [2 3] 4)
      [a b c d])
    [1 2 3 4]
    )
  
   (eq_
    (dbind
      (a (b c) d) [1 [2 3] 4]
      [a b c d])
    [1 2 3 4]
    )
  )
