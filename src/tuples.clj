;; Antonio Garrote Hernandez 2009

;; Rough implementation of pattern matching for clojure

(ns tuples)


;; Examples

;; (def v '(1 2 (3 4) 5))
;; (tuples/match (z u (s1 s2) t) v (println s1))
;; > 3
;; (tuples/match (z u s t) v (println s))
;; > (3 4)
;;(tuples/match (z u (s1 s2) t) (1 2 (3 4) 5) (println s2))
;; > 4
;;(tuples/match (3 u (s1 s2) t) (1 2 (3 4) 5) (println s2))
;; matching exception

;; (def v '(2 2))
;; (tuples/case (v 2)
;;              (z 1) (println (str "a " z))
;;              (z 2) (println (str "b " z))
;;              t     :error)
;; > b (2 2)
;; (tuples/case (v 3)
;;              (z 1) (println (str "a " z))
;;              (z 2) (println (str "b " z))
;;              t     :error)
;;> :error



;; private forms

(defmacro defined-symbol-string? [s]
  `(let [parts# (.split ~s "/")]
     (if (= (alength parts#) 2)
       (contains? (ns-map (symbol (aget parts# 0)))
                  (symbol (aget parts# 1)))
       (contains? (ns-map *ns*) (symbol ~s)))))

;; Since it is necessary to know at run time if
;; the symbol in the pattern is bound or not, this
;; is a dirty way of checking if the value is defined
(defn safe-eval [str]
  (if (defined-symbol-string? str)
    (eval (symbol str))
    :tuples/tmp_undef))

(defmacro match-lists [f value pattern-orig-size & patterns]
  (if (not (nil? patterns))
    (let [p# (first patterns)
          rp# (rest patterns)
          acum# (gensym "mzl-")]
      (if (list? value)
        `(let [~acum# (nth '~value (- ~pattern-orig-size (count '~patterns)))]
           (match ~p# ~acum# (match-lists ~f  ~value ~pattern-orig-size ~@rp#)))
        `(let [~acum# (nth ~value (- ~pattern-orig-size (count '~patterns)))]
           (match ~p# ~acum# (match-lists ~f  ~value ~pattern-orig-size ~@rp#)))))
    `~f))


;; public API


(defmacro match [a b f]
   "Matches values and lists acting as tuples."
   (cond
     (symbol? a) (if (not (= \_ (.charAt (str a) 0)))
                   `(let [~a (safe-eval (str '~a))]
                    (if (not (= ~a :tuples/tmp_undef))
                      (if (=~a  ~b)
                        ~f
                        (throw (Exception. "matching error")))
                      (if (list? '~b)
                        (let [~a '~b]
                          ~f)
                        (let [~a ~b]
                          ~f))))
                   `~f)
     (list? a) `(match-lists ~f ~b (count '~a) ~@a)
     true      `(if (= ~a ~b)
                  ~f
                  (throw (Exception. "matching error")))
     ))


(defmacro case [v & ps]
  "Case value of patterns and actions construction."
  (if (= (count ps) 2)
    (let [p# (nth ps 0)
          pr# (first (rest ps))]
      `(match ~p# ~v ~pr#))
    (let [p# (nth ps 0)
          pr# (first (rest ps))
          psp# (drop 2 ps)]
      `(try (match ~p# ~v ~pr#)
            (catch Exception e# (case ~v ~@psp#))))))


;; tests

(use 'clojure.test)

(deftest tautology-match-test
  (is (= true
         (match true true true))))

(deftest contradiction-match-test
  (is (= false
         (try (match true false true)
              (catch Exception e false)))))

(deftest single-assignation-test
  (is (= 1
         (match a 1 a))))

(deftest single-assignation-b-test
  (let [a (+ 1 2)]
    (is (= true
           (match 3 a true)))))

(deftest single-assignation-in-function-test
  (let [f (fn [a] (match b a b))]
    (is (= 1 (f 1)))
    (is (= 2 (f 2)))
    (is (= 3 (f (+ 1 2))))))

(deftest list-assignment-a-test
  (is (= '(1 2)
         (match a (1 2) a))))

(deftest list-assignment-b-test
  (is (= 1
         (match (a 2) (1 2) a))))

(deftest list-assigment-function-test
  (let [f (fn [x] (match (a b) x b))]
    (is (= 3
           (f '(1 3))))))

(deftest case-a-test
  (let [a 1]
    (is (= '(:ok :b)
           (case (:ok 2)
                 (b 1) (list b :a)
                 (b 2) (list b :b))))))

(deftest dont-care-pattern-test
  (is (= 2
         (match (_ b) ((1 3) 2) b)))
    (is (= 2
           (match (_dont-care b) ((1 3) 2) b))))