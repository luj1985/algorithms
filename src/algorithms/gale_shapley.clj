(ns algorithms.gale-shapley
  (:require [clojure.pprint :refer [pprint]]))

(defn ranking-by [preference]
  #(.indexOf preference %))

(defn is-better-choice? [[id1] [id2] [_ preference]]
  (let [rank (ranking-by preference)]
    (< (rank id1) (rank id2))))

(defn sort-by-preference [[_ preference] women]
  (sort-by (comp (ranking-by preference) first) (seq women)))

(defn propose-best [man women engagement]
  (loop [women (sort-by-preference man women)]
    (if-let [[woman & women] (seq women)]
      (if-let [existing (engagement woman)]
        (if (is-better-choice? existing man woman)
          (recur women)
          woman)
        woman))))

(defn exclude-ex [man]
  ;; the last engaged woman must stay at the first of his preference list
  (let [[id preference] man]
    [id (rest preference)]))

;;; Stable Matching Problem (https://en.wikipedia.org/wiki/Stable_marriage_problem)
(comment
  ;; sample men preference list
  {:a [1 2 3 4]
   :b [2 3 4 1]
   :c [3 2 1 4]
   :d [2 4 1 3]}

  ;; sample women preference list
  {1 [:a :b :c :d]
   2 [:a :d :c :b]
   3 [:b :a :d :c]
   4 [:c :a :d :b]})

(defn gale-shapley [men women]
  ;;; first iteration, all men are available
  (loop [men men engagement {}]
    (if-let [[man & men] (seq men)]
      ;; at least one woman response
      ;; either she is available, or think this man is a better choice
      ;; if no one reponse, it must be at the end of the iterations
      (if-let [woman (propose-best man women engagement)]
        ;; if this woman has engaged before, the man became an ex
        (if-let [abandaned (engagement woman)]
          (recur (cons (exclude-ex abandaned) men) (assoc engagement woman man))
          (recur men (assoc engagement woman man))))
      (sort
       (map (comp vec (partial map first) reverse) engagement)))))

;;;;;;;;;;;;;;;;;;;; Generate test data ;;;;;;;;;;;;;;;;;
(comment

  (defn generate-preferences [coll1 coll2]
    (for [x coll1]
      [x (shuffle coll2)]))

  (defn generate-keyword-sequence [n]
    (let [start (int \a)]
      (map (comp keyword str char) (range start (+ start n)))))

  (defn generate-number-sequence [n]
    (let [start 1]
      (range start (+ start n))))

  (defn print-gale-shapley [men women]
    (let [engagement (gale-shapley men women)]
      (pprint men)
      (pprint women)
      (pprint engagement)))

  (let [men [[:a [3 1 2 4 5]]
             [:b [1 5 2 4 3]]
             [:c [5 4 2 3 1]]
             [:d [5 2 1 4 3]]
             [:e [4 5 2 1 3]]]
        women [[1 [:b :c :a :d :e]]
               [2 [:e :b :a :c :d]]
               [3 [:c :b :a :d :e]]
               [4 [:d :c :b :a :e]]
               [5 [:d :c :a :b :e]]]]
    (println "-------------------------------------------")
    (print-gale-shapley men women))

  (let [size 5
        men (generate-keyword-sequence size)
        women (generate-number-sequence size)
        men-preferences (generate-preferences men women)
        women-preferences (generate-preferences women men)]
    (println "-------------------------------------------")
    (print-gale-shapley men-preferences women-preferences)))

