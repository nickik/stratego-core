(ns stratego-core.core
  (:require [clojure.core.match :as m])
  (:use [clojure.pprint]))

(def board-side 10)
(def player-side 4)

(def all-pos (set (for [x (range 0 board-side)
                        y (range 0 board-side)]
                    [x y])))

(def water-ver [2 3 6 7])
(def water-hor [4 5])

(def all-water (set (for [x water-hor
                          y water-ver]
                      [x y])))

(def middle-land (set (for [x water-hor
                            y (vec (clojure.set/difference (set (range 0 board-side))
                                                           (set water-ver)))]
                    [x y])))

(def middle-part (set (concat all-water middle-land)))

(def player-pos (set (for [x (range 0 player-side)
                           y (range 0 board-side)]
                        [x y])))

(def player2-pos (set (for [x (range 6 board-side)
                            y (range 0 board-side)]
                        [x y])))

(def all-land (set (concat player-pos player2-pos middle-land)))

(def gound [:water :land])

(def figures {:marshal {:count 1
                        :rank 10}
              :general {:count 1
                        :rank 9}
              :colonel {:count 2
                        :rank 8}
              :major {:count 3
                      :rank 7}
              :captain {:count 4
                        :rank 6}
              :lieutenant {:count 4
                           :rank 5}
              :sergeant {:count 4
                         :rank 4}
              :miner {:count 5
                      :rank 3}
              :scout {:count 8
                      :rank 2}
              :spy {:count 1
                      :rank 1}
              :bomb {:count 6
                      :rank \B}
              :flag {:count 1
                      :rank \F}})

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn wins-against [attacker defender]
  (let [a-rank (:rank (attacker figures))
        d-rank (:rank (defender figures))]
    (m/match [a-rank d-rank]
         [1 10] true
         [3 \B] true
         [_ \B] false
         :else (> a-rank d-rank))))

(def unit {:type nil
           :color nil})

(defn creat-field [pos]
  {pos {:ground (if (some #{pos} all-land)
                        :land
                        :water)
        :unit nil}})

(def full-empty-board (apply merge (map #(creat-field %)
                                  (sort all-pos))))

(defn set-unit [field pos unit]
  (assoc-in field [pos :unit] unit))

(defn get-unit [field pos]
  (get-in field [pos :unit]))

(defn move-unit [field from to]
  (let [unit (get-unit field from)
        to-unit (get-unit field to)]
    (if unit
      (-> field
          (set-unit to unit)
          (set-unit from nil))
      :field-empty)))

(def f full-empty-board)

(set-unit full-empty-board [2 1] :marshal)

(move-unit (set-unit full-empty-board [2 1] :marshal)
            [2 1]
            [9 9])

(defn out-of-bound-field-filter [positions]
  (filter #(some #{%} all-land) positions))

(defn position-filter [positions pos-to-filter-out]
  (clojure.set/difference (set positions)
                          (set pos-to-filter-out)))

(defn fun [n]
    (range (dec n) (+ 2 n)))

(defn find-possible-moves [[x y]]
  (if (some #{[x y]} all-land)
    (let [t-pos (concat (map #(vector x %) (fun y))
                        (map #(vector % y) (fun x)))
          filter-fn (comp out-of-bound-field-filter
                          position-filter)]
      (filter-fn t-pos [[x y]]))
    []))

