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
         [_ \F] true
         :else (> a-rank d-rank))))

(def unit {:type nil
           :color nil})

(defn creat-field [pos]
  {pos {:ground (if (some #{pos} all-land)
                        :land
                        :water)
        :unit unit}})

(def full-empty-board (apply merge (map creat-field
                                        all-pos)))

(defn set-unit [field pos unit]
  (assoc-in field [pos :unit] unit))

(defn get-unit [field pos]
  (get-in field [pos :unit]))

(defn can-move [type]
  (number? (:rank (type figures))))

(defn check-move [field from to]
  (let [from-unit (get-unit field from)
        to-unit (get-unit field to)]
    (if from-unit
      (if (can-move (:type from-unit))
        (if (some #{to} (find-possible-move field from))
          (-> field
              (set-unit , to unit)
              (set-unit , from nil))
          (assoc field :check-move-error {:from from :to to :type :not-in-possible-move}))
        (assoc field :check-move-error {:form from :to to :type :from-unit-cant-move}))
      (assoc field :check-move-error {:from from :to to :type :no-from-unit}))))

(defn move-unit [field from to]
  (when (check-move field from to)
     (let [unit (get-unit field from)
                 to-unit (get-unit field to)]
             (-> field
                 (set-unit , to unit)
                 (set-unit , from nil)))))


(defn field-occ-filter [field unit positions]
  (filter #(= (:color (get-unit field %))
              (:color unit))
          positions))


(defn out-of-bound-field-filter [positions]
  (filter #(some #{%} all-land) positions))

(defn position-filter [positions pos-to-filter-out]
  (clojure.set/difference (set positions)
                          (set pos-to-filter-out)))

(defn fun [n]
    (range (dec n) (+ 2 n)))

(defn fun-scout [n]
  (range (- n board-side) (+ n board-side)))

(defn has-unit-filter [field positions]
  (filter #(= nil
              (:type (get-unit field %)))
          positions))

(defn can-walk-over [field position]
  (->> position
       (out-of-bound-field-filter)
       (has-unit-filter field)))

(defn can-walk-on [field [x y] color]
  (let [unit (get-unit field [x y])]
    (not= (:color unit) color)))


(defn theory-possible-positions  [[x y] f]
  (concat (map #(vector x %) (f y))
          (map #(vector % y) (f x))))


(defn scout-search [field [x y] direction]
  (loop [cord [x y] possible []]
    (let [consider-field (map + cord direction)]
      (if (empty? (can-walk-over field [consider-field]))
        (recur consider-field (conj possible consider-field))
        (if (can-walk-on field consider-field (:color (get-unit field [x y])))
          (conj possible consider-field)
          possible)))))


(scout-search full-empty-board [3 3] [1 0])


(defn pos-scout
  "Start for searches in for diffrent direction until it encounters first non-passable objet"
  [field [x y]]
  (map (partial scout-search field [x y]) [[1  0][(- 1) 0][0 1][0 (- 1)]]))



;;(possible-scout-moves full-empty-board [3 3])

  (comment
    (defn find-possible-moves [field [x y]]
      (if (= (get-unit field [x y]) :scout)

        (if (some #{[x y]} all-land)
          (if (= (get-unit field [x y]) :scout)
            (possible-scout-moves field [x y])
            (let [t-pos (theory-possible-positions [x y] fun)
                  filter-fn (comp (partial field-occ-filter field (get-unit field [x y]))
                                  out-of-bound-field-filter
                                  position-filter)]
              (filter-fn t-pos [[x y]]))
            []))))

    (defn player-win [board]
      true))


