(ns aoc.y2018.d08.ClashTheBunny
  (:refer-clojure :exclude [read-string format])
  (:require
   [aoc.utils :as u :refer [deftest read-string format]]
   [aoc.y2018.d08.data :refer [test-input test-answer-1 input answer-1 answer-2]]
   [clojure.test :refer [is testing]]
   [clojure.string :as s]
   [editscript.core :as es]
   [editscript.edit :as e]
   [editscript.diff.quick :as q]
   [editscript.diff.a-star :as a]
   [editscript.util.common :as com
      #?@(:cljs [:include-macros true])]
   [puget.printer :as pug]))

(defn parse [tree-string]
  (mapv read-string (s/split (s/trim tree-string) #" ")))

(def test-tree {:id 1
                :count-children 2
                :count-metadata 3
                :metadata [1 1 2]
                :children [{:id 2
                            :count-children 0
                            :count-metadata 3
                            :metadata [10 11 12]
                            :children nil}
                           {:id 3
                            :count-children 1
                            :count-metadata 1
                            :metadata [2]
                            :children [{:id 4
                                        :count-children 0
                                        :count-metadata 1
                                        :children nil
                                        :metadata [99]}]}]})

(defn length-of-node [tree-array child]
  (if (< child 0)
    0
    (cond
      (= 0 child)
      (if (= 0 (first tree-array))
          (+ 2 (second tree-array))
          (+ 2 (second tree-array)
             (length-of-node (drop 2 tree-array) 0)))
      (= 1 child)
      (length-of-node (drop (length-of-node tree-array (dec child))
                            tree-array)
                      (dec child))
      (< 1 child)
      (do
       (println "More than one children")
       (pug/cprint tree-array)
       (pug/cprint child)))))

(defn gen-nodes [tree-array id]
  (let [count-children (first tree-array)
        count-metadata (second tree-array)
        rest-of-tree   (drop 2 tree-array)
        children  (into [] (for [child (range count-children)]
                            (let [len (length-of-node rest-of-tree child)]
                               (gen-nodes (drop (length-of-node rest-of-tree
                                                                (dec child))
                                                rest-of-tree)
                                          (+ child (inc id))))))
        children   (if (= 0 (count children)) nil children)]
      {:id id
       :count-children count-children
       :count-metadata count-metadata
       :metadata (into [] (take-last count-metadata tree-array))
       :children children}))

(defn length-of-children [children]
  (flatten
   (for [child children]
    [(:head child)
     (:meta child)
     (length-of-children (:children child))])))

(defn take-2-or-non-child [tree-array subtrees-left children]
  (println (take 30 tree-array))
  (if (not= 0 (count tree-array))
    (let [items    (second tree-array)
          subtrees (first tree-array)]
      (if (= 0 subtrees)
        (do
         ;; (println "hit a leaf")
         ;; (pug/cprint {:head (take 2 tree-array) :meta (take items (drop 2 tree-array))})
         [{:head (take 2 tree-array) :meta (take items (drop 2 tree-array))}])
        (let [root
               {:head (take 2 tree-array) :count-children subtrees
                :count-meta items         :meta (take-last items tree-array)}]
             (let [current-children (take-2-or-non-child tree-array subtrees-left children)
                   items-of-curr-children (length-of-children current-children)]
               ;; (println "doing a child")
               (pug/cprint current-children)
               (pug/cprint items-of-curr-children)
               (pug/cprint subtrees-left)
               (pug/cprint children)
               (if (and (< 0 subtrees-left)
                        (< 0 (count tree-array)))
                (recur (drop (count items-of-curr-children) tree-array)
                       (dec subtrees-left)
                       (conj children current-children))
                {:root root :children children})))))))

(defn build-tree [tree-string]
  (-> (parse tree-string)
      (#(take-2-or-non-child % (first %) []))
      pug/cprint))

(defn sum-metadata [tree]
  (reduce + (concat (:metadata tree)
                    (for [child (:children tree)]
                      (sum-metadata child)))))

(defn solve-1 [input]
  (sum-metadata (build-tree input)))


(defn solve-2 [input])
  ;; TODO


(deftest part-1
  #_(is (= (str [])
           (str (e/get-edits (a/diff test-tree (build-tree test-input))))))
  #_(is (= test-answer-1
           (solve-1 test-input)))
  (is (= answer-1
         (solve-1 test-input))))

(deftest part-2
  #_(is (= answer-2
           (solve-2 input))))
