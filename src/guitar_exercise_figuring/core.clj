(ns guitar-exercise-figuring.core
  "FIXME: my new org.corfield.new/scratch project."
  #_(:require [clojure.math.combinatorics :as combo]))

;; (defn exec
;;   "Invoke me with clojure -X guitar-exercise-figuring.guitar-exercise-figuring/exec"
;;   [opts]
;;   (println "exec with" opts))

;; (defn -main
;;   "Invoke me with clojure -M -m guitar-exercise-figuring.guitar-exercise-figuring"
;;   [& args]
;;   (println "-main with" args))

(defn priority->priority-weight [priority priority-max]
  (let [weight-per-priority (/ 1 priority-max)
        priority-weight (+ weight-per-priority
                           (- 1 (* priority weight-per-priority)))]
    priority-weight))

(defn priorities->priority-weights [priorities]
  (let [priority-set (set priorities)
        priority-count (count priority-set)
        priority-max (apply max priorities)]
    (if-not (= priority-count priority-max)
      (throw (ex-info "priority count should equal priority-max"
                      {:priority-count priority-count
                       :priority-max priority-max}))
      (->> priority-set
           (map #(vector % (priority->priority-weight % priority-max)))
           (into {})))))

(comment

  (priority->priority-weight 3 9)
  (priority->priority-weight 1 9)
  (priority->priority-weight 2 9)
  (priority->priority-weight 5 9)
  (priority->priority-weight 9 9)

  (- 1 (/ 1 9))

  )

