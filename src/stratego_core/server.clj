(ns stratego-core.server
  (:require [clojure.core.match :as m])
  (:use [clojure.pprint]
        [stratego-core.core]))

(def games (atom {}))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn new-game [games start-field]
  (let [uuid (uuid)]
    (do
      (swap! games assoc uuid full-empty-board)
      uuid)))



;; From Player 1: POST game/<start-field> --> /game/uuid

;; From Player 2: PUT  game/uuid/<start field 2>

;; From Player 1: PUT game/uuid/move {:from :to} -> retunrs [200 board] or [400 board]
;; From Player 2: PUT game/uuid/move {:from :to} -> retunrs [200 board] or [400 board]

;; and so on until

;; From Player 2: PUT game/uuid/move {:from :to} -> retunrs [200 :win board]

