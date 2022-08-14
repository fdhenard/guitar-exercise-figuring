(ns guitar-exercise-figuring.triads
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.math.combinatorics :as combo]
            [guitar-exercise-figuring.core :as core]
            [guitar-exercise-figuring.chords :as chords]))

(def string-set-priority
  [{:string-set 1
    :priority 1}
   {:string-set 2
    :priority 2}
   {:string-set 3
    :priority 3}
   {:string-set 4
    :priority 4}])

(def string-set-priorities->weights
  (core/priorities->priority-weights (map :priority string-set-priority)))

(comment

  string-set-priorities->weights

  )

(def string-sets
  (map #(assoc % :priority-weight (string-set-priorities->weights (:priority %)))
       string-set-priority))

(comment

  string-sets

  )


(def chords (map #(select-keys % [:chord-name :combined-priority-weight]) (chords/chords)))

(comment

  chords

  

  (combo/cartesian-product chords string-sets)

  )

(defn combine-cart-prod [[{:keys [chord-name]
                           chord-priority-weight :combined-priority-weight}
                          {:keys [string-set]
                           string-set-priority-weight :priority-weight}]]
  {:chord-name chord-name
   :chord-priority-weight chord-priority-weight
   :string-set string-set
   :string-set-priority-weight string-set-priority-weight
   :combined-priority-weight (+ chord-priority-weight string-set-priority-weight)})

(defn triads []
  (let [cart-prod (combo/cartesian-product chords string-sets)
        combined (map combine-cart-prod cart-prod)
        sorted (sort-by :combined-priority-weight > combined)]
    sorted))

(comment

  (triads)

  )

(defn make-csv! []
  (let [f-path "target/triads.csv"
        _ (io/make-parents f-path)
        columns [:chord-name :string-set :combined-priority-weight]
        headers (map name columns)
        rows (mapv #(mapv % columns) (triads))]
    (with-open [file (io/writer f-path)]
      (csv/write-csv file (cons headers rows)))))

(comment

  (make-csv!)

  )
