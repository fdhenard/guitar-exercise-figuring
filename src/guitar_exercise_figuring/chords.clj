(ns guitar-exercise-figuring.chords
  (:require [clojure.math.combinatorics :as combo]
            [guitar-exercise-figuring.core :as core]))


(def guitar-key-priority
  [{:key "C"
    :priority 3}
   {:key "F"
    :priority 5}
   {:key "Bb"
    :priority 6}
   {:key "Eb"
    :priority 6}
   {:key "Ab"
    :priority 7}
   {:key "Db"
    :priority 8}
   {:key "C#"
    :priority 9}
   {:key "Gb"
    :priority 9}
   {:key "F#"
    :priority 9}
   {:key "B"
    :priority 5}
   {:key "E"
    :priority 4}
   {:key "A"
    :priority 5}
   {:key "D"
    :priority 2}
   {:key "G"
    :priority 1}])


(def key-priorities->weights
  (core/priorities->priority-weights (map :priority guitar-key-priority)))

(comment

  key-priorities->weights

  )

(def chord-degree-priority
  [{:chord-degree 1
    :priority 1}
   {:chord-degree 2
    :priority 2}
   {:chord-degree 3
    :priority 3}
   {:chord-degree 4
    :priority 1}
   {:chord-degree 5
    :priority 1}
   {:chord-degree 6
    :priority 2}
   {:chord-degree 7
    :priority 4}])

(def chord-degree-priorities->weights
  (core/priorities->priority-weights (map :priority chord-degree-priority)))

(comment

  chord-degree-priorities->weights

  )

(def chord-degree->triad-type
  {1 "Maj"
   2 "Min"
   3 "Min"
   4 "Maj"
   5 "Maj"
   6 "Min"
   7 "Min7b5"})

(def key-interval->note
  {["C" 1] "C"
   ["C" 2] "D"
   ["C" 3] "E"
   ["C" 4] "F"
   ["C" 5] "G"
   ["C" 6] "A"
   ["C" 7] "B"

   ["F" 1] "F"
   ["F" 2] "G"
   ["F" 3] "A"
   ["F" 4] "Bb"
   ["F" 5] "C"
   ["F" 6] "D"
   ["F" 7] "E"

   ["Bb" 1] "Bb"
   ["Bb" 2] "C"
   ["Bb" 3] "D"
   ["Bb" 4] "Eb"
   ["Bb" 5] "F"
   ["Bb" 6] "G"
   ["Bb" 7] "A"

   ["Eb" 1] "Eb"
   ["Eb" 2] "F"
   ["Eb" 3] "G"
   ["Eb" 4] "Ab"
   ["Eb" 5] "Bb"
   ["Eb" 6] "C"
   ["Eb" 7] "D"

   ["Ab" 1] "Ab"
   ["Ab" 2] "Bb"
   ["Ab" 3] "C"
   ["Ab" 4] "Db"
   ["Ab" 5] "Eb"
   ["Ab" 6] "F"
   ["Ab" 7] "G"

   ["Db" 1] "Db"
   ["Db" 2] "Eb"
   ["Db" 3] "F"
   ["Db" 4] "Gb"
   ["Db" 5] "Ab"
   ["Db" 6] "Bb"
   ["Db" 7] "C"

   ["C#" 1] "C#"
   ["C#" 2] "D#"
   ["C#" 3] "E#"
   ["C#" 4] "G#"
   ["C#" 5] "A#"
   ["C#" 6] "B#"
   ["C#" 7] "C#"

   ["Gb" 1] "Gb"
   ["Gb" 2] "Ab"
   ["Gb" 3] "Bb"
   ["Gb" 4] "Cb"
   ["Gb" 5] "Db"
   ["Gb" 6] "Eb"
   ["Gb" 7] "F"

   ["F#" 1] "F#"
   ["F#" 2] "G#"
   ["F#" 3] "A#"
   ["F#" 4] "B"
   ["F#" 5] "C#"
   ["F#" 6] "D#"
   ["F#" 7] "E#"

   ["B" 1] "B"
   ["B" 2] "C#"
   ["B" 3] "D#"
   ["B" 4] "E"
   ["B" 5] "F#"
   ["B" 6] "G#"
   ["B" 7] "A#"

   ["E" 1] "E"
   ["E" 2] "F#"
   ["E" 3] "G#"
   ["E" 4] "A"
   ["E" 5] "B"
   ["E" 6] "C#"
   ["E" 7] "D#"

   ["A" 1] "A"
   ["A" 2] "B"
   ["A" 3] "C#"
   ["A" 4] "D"
   ["A" 5] "E"
   ["A" 6] "F#"
   ["A" 7] "G#"

   ["D" 1] "D"
   ["D" 2] "E"
   ["D" 3] "F#"
   ["D" 4] "G"
   ["D" 5] "A"
   ["D" 6] "B"
   ["D" 7] "C#"

   ["G" 1] "G"
   ["G" 2] "A"
   ["G" 3] "B"
   ["G" 4] "C"
   ["G" 5] "D"
   ["G" 6] "E"
   ["G" 7] "F#"

   })

(defn combine-cart-prod [[{:keys [key]
                           key-priority :priority}
                          {:keys [chord-degree]
                           chord-degree-priority :priority}]]
  (let [chord-note (key-interval->note [key chord-degree])
        chord-triad-type (chord-degree->triad-type chord-degree)
        key-priority-weight (key-priorities->weights key-priority)
        chord-degree-priority-weight (chord-degree-priorities->weights chord-degree-priority)]
   {:key key
    :key-priority key-priority
    :chord-degree chord-degree
    :chord-degree-priority chord-degree-priority
    :chord-name (str chord-note chord-triad-type)
    :key-priority-weight key-priority-weight
    :chord-degree-priority-weight chord-degree-priority-weight
    :combined-priority-weight (+ key-priority-weight chord-degree-priority-weight)}))


(defn chords []
  (let [cart-prod-res (combo/cartesian-product guitar-key-priority chord-degree-priority)
        cart-prod-combined (map combine-cart-prod cart-prod-res)
        grouped (group-by :chord-name cart-prod-combined)
        something (map (fn [[chord-name chord-derivations]]
                         (let [combined-weights (map :combined-priority-weight chord-derivations)]
                           {:chord-name chord-name
                            :combined-priority-weight (reduce + combined-weights)
                            :chord-derivations chord-derivations}))
                       grouped)]
    something))


(comment

  (def the-dater (chords))

  the-dater

  (count the-dater)

  (def chords (map :chord-name the-dater))
  chords

  (count (set chords))


  (group-by :chord-name the-dater)

  (sort-by :combined-priority-weight > the-dater)

  (def chord-names-in-order
    (map :chord-name (sort-by :combined-priority-weight > the-dater)))
  chord-names-in-order

  (count chord-names-in-order)

  )
