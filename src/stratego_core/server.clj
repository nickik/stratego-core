(ns stratego-core.server
  (:require [clojure.core.match :as m]
            [compojure.core :refer [defroutes ANY]])
  (:use [clojure.pprint]
        [stratego-core.core]
        [liberator.core :only [defresource request-method-in]]))

(def games (atom {}))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn new-game
  ([] (new-game full-empty-board))
  ([start-field]
   (let [uuid (uuid)]
     (do
       (swap! games assoc uuid {:board start-field})
       uuid))))

(defresource main
  :available-media-types ["text/html"]
  :handle-ok (str  "<h3> Stratego Server </h3>
                   <p> Play the old school board game stratego agaisnt
                   your friends or enemys. </p>
                   <p> Source Code and Documnetation is here:
                   <a href=\")https://github.com/nickik/stratego-core\"> stratego-core </a> </p>
                   <p> Allready existing games:"  (keys @games) "</p>"))


(defresource game
  :allowed-methods [:post :get]
  :available-media-types ["text/html"]
  :handle-ok (fn [ctx]
               (str "Any Game " (keys @games)))
  :post! (fn [ctx]  (new-game))

  :post-redirect? false)


(defroutes app
  (ANY "/" [] main)
  (ANY "/game" [] game))

(def handler (-> app))

;; From Player 1: POST game/<start-field> --> /game/uuid

;; From Player 2: PUT  game/uuid/<start field 2>

;; From Player 1: PUT game/uuid/move {:from :to} -> retunrs [200 board] or [400 board]
;; From Player 2: PUT game/uuid/move {:from :to} -> retunrs [200 board] or [400 board]

;; and so on until

;; From Player 2: PUT game/uuid/move {:from :to} -> retunrs [200 :win board]

