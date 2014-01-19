(ns stratego-core.core-test
  (:require [clojure.test :refer :all]
            [stratego-core.core :refer :all]))

(deftest wins-against-test-true
  (are [fight] (partial = true)
       (wins-against :scout :spy)
       (wins-against :spy :marshal)
       (wins-agasitn :marshal :spy)
       (wins-against :marshal :general)
       (wins-against :marshal :miner)
       (wins-agaisnt :miner :scout)
       (wins-agaisnt :miner :bomb)
       (wins-agaisnt :captain :sergeant)
       (wins-against :colonel :major))
  (are [fight] (partial = false)
       (wins-against :spy :spy)
       (wins-against :marshal :marshal)
       (wins-against :miner :marshal)
       (wins-agasint :scout :bomb)
       (wins-agasint :spy :bomb)))

(deftest board-composition-test
  (is (= all-pos
         (set (concat player-pos
                      player2-pos
                      all-water
                      middle-land))))
  (is (= all-pos
         (set (concat all-water
                      all-land))))
  (is (= all-land
         (set (concat player-pos
                      player2-pos
                      middle-land)))))

(deftest set-unit-test
  (is (= :marshal
         (get-unit (set-unit full-empty-board [0 0] :marshal) [0 0]))))

(deftest move-unit-move-test
  (let [new-field (move-unit (set-unit full-empty-board
                                [2 1]
                                :marshal)
                              [2 1]
                              [3 3])
        ]
    (is (= (get-unit new-field [3 3])
           :marshal))))

(deftest move-unit-move-test
  (is (= (move-unit full-empty-board
                              [2 1]
                              [3 3])
         :field-empty)))

(deftest find-possible-moves-test
   (are [x y] (= x y)
        (find-possible-moves [0 0]) [[1 0] [0 1]]
        (find-possible-moves [1 1]) [[2 1] [1 0] [0 1] [1 2]]
        (find-possible-moves [9 9]) [[9 8] [8 9]]
        (find-possible-moves [-1 0]) []
        (find-possible-moves [-153 90]) []
        (find-possible-moves (rand-nth (vec all-water))) []
        (find-possible-moves [3 3]) [[3 2] [2 3] [3 4]]))