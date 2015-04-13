(ns contextual.core-test
  (:require #?@(:cljs [[cljs.test :as t :refer-macros [deftest is]]
                       [cljs.test.check :as tc]
                       [cljs.test.check.generators :as gen]
                       [cljs.test.check.properties :as prop :include-macros true]
                       [cljs.test.check.cljs-test :refer-macros [defspec]]]
                :clj [[clojure.test :as t :refer [deftest is]]
                      [clojure.test.check :as tc]
                      [clojure.test.check.generators :as gen]
                      [clojure.test.check.properties :as prop]
                      [clojure.test.check.clojure-test :refer [defspec]]])
            [contextual.core :as c]))

(deftest contextual-map-basics
  (let [c (c/contextualize {:foo :bar} [:a :b])]
    (is (= [:a :b] (c/context c)))
    (is (= {:foo :bar} (c/decontextualize c)))
    (is (= c {:foo :bar}))
    (is (= {:foo :bar} c))
    (is (= (hash {:foo :bar}) (hash c)))))

(deftest contextual-map-traversal
  (let [c (c/contextualize {:a {:b {:c 0}}})]
    (is (= [] (c/context c)))
    (is (= [:a] (c/context (:a c))))
    (is (= [:a :b] (c/context (-> c :a :b))))))

(deftest contextual-map-lookup
  (let [c (c/contextualize {:a {:b {:c 0}}})
        check (fn [v]
                (is (= [:a] (c/context v)))
                (is (= {:b {:c 0}} (c/decontextualize v))))]
    (check (:a c))
    (check (get c :a))
    (check (c :a))
    (check (apply c [:a]))
    (check (get-in c [:a]))
    (check (second (first (seq c))))))

(deftest contextual-vec-basics
  (let [c (c/contextualize [:foo :bar] [:a :b])]
    (is (= [:a :b] (c/context c)))
    (is (= [:foo :bar] (c/decontextualize c)))
    (is (= c [:foo :bar]))
    (is (= [:foo :bar] c))
    (is (= (hash [:foo :bar]) (hash c)))))

(deftest contextual-vec-traversal
  (let [c (c/contextualize [[:a :b] [:c :d] [:e :f]])]
    (is (= [] (c/context c)))
    (is (= [:a :b] (first c)))
    (is (= [0] (c/context (first c))))))

(deftest contextual-vec-lookup
  (let [c (c/contextualize [[:a] [:b] [:c] [:d]])]
    (is (= [[0] [1] [2] [3]] (map c/context c)))
    (is (= [2] (c/context (c 2))))
    (is (= [2] (c/context (nth c 2))))
    (is (= [2] (c/context (get c 2))))
    (is (= [2] (c/context (first (filter #(= [:c] %) c)))))))

(deftest mixed-traversal
  (let [c (c/contextualize {:a [nil {:b [nil nil {:c [4]}]}]})]
    (is (= [:a 1 :b 2 :c] (c/context (get-in c [:a 1 :b 2 :c]))))))

(defn check-children
  [parent path k v]
  (if (or (map? v) (vector? v))
    (and (satisfies? c/Contextual v)
         (satisfies? c/Contextual (get parent k))
         (satisfies? c/Contextual (parent k))
         (= path (c/context v))
         (cond
          (map? v) (every? (fn [[child-k child-v]]
                             (check-children v (conj path child-k) child-k child-v)) v)
          (vector? v) (every? identity
                              (map-indexed (fn [idx child]
                                             (check-children v (conj path idx) idx child))
                                           v))))
    true))

(defspec context-is-passed-correctly 30
  (prop/for-all [m (gen/map gen/any gen/any)]
                (let [c (c/contextualize m)]
                  (every? (fn [[k v]]
                            (check-children c [k] k v))
                          c))))




