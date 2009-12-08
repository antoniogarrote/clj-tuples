(ns test-clj-tuples
  (:use [clojure.test]
        [clj-tuples])
  (:import (clj_tuples PatternMatchException)))

;; tests

;;(use 'clojure.test)

(deftest tautology-match-test
  (is (= true
         (match true true true))))

(deftest contradiction-match-test
  (is (= false
         (try (match true false true)
              (catch PatternMatchException e false)))))

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

(deftest case-b-test
  (let [a 1]
    (is (= '(2 :b)
           (case (:ok 2)
                 (:ok a b) (list b :a)
                 (:ok b)   (list b :b))))))

(deftest case-c-test
  (let [a 1]
    (is (= '(2 :b)
           (case (:ok 2)
                 "ok"      (list :a)
                 (:ok b)   (list b :b))))))

(deftest dont-care-pattern-test
  (is (= 2
         (match (_ b) ((1 3) 2) b)))
    (is (= 2
           (match (_dont-care b) ((1 3) 2) b))))

(deftest vector-to-list-test
  (is (= '(1 2 3)
         (vector-to-list [1 2 3])))
  (is (= '()
         (vector-to-list []))))

(deftest match-vectors-simple-test
  (is (= [1 2]
         (match a [1 2] a)))
  (is (= true
         (match [1 2] [1 2] true)))
  (is (= false
         (try (match [2 3] [1 2] true)
              (catch PatternMatchException e false))))
  (is (= false
         (try (match [1 2] (1 2) true)
              (catch PatternMatchException e false)))))

(deftest match-vectors-complex-test
  (is (= 1
         (match (5 [a b] 4) (5 [1 2] 4) a)))
  (is (= 1
         (match [5 [a b] 4] [5 [1 2] 4] a)))
  (is (= [1 2]
         (match (5 a 4) (5 [1 2] 4) a)))
  (is (= [1 2]
         (match [5 a 4] [5 [1 2] 4] a))))

(deftest case-vectors-test
  (is (= 1
         (case (5 [1 2] 4)
               (4 a 5) a
               (5 [a b] 4) a))))

(deftest map-match-1-test
  (is (= 1
         (let [m {:a 1 :b "hola"}]
           (match {:a a :b b} m a))))
  (is (= 1
         (let [m {:a 1 :b "hola"}]
           (match {:a a} m a))))
  (is (= false
         (try (match {:a 2} {:a 1} true)
              (catch PatternMatchException e false)))))

(deftest exceptions-1-test
  (is (= true
         (try
          (try (match true false 0)
               (catch PatternMatchException e1 true))
          (catch Exception e2 false))))
  (is (= false
         (try
          (try (match true true (throw (Exception.)))
               (catch PatternMatchException e1 true))
          (catch Exception e2 false)))))