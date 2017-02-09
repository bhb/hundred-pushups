(ns hundred-pushups.core
  (:require
    [clojure.spec.test :as st]
    [clojure.spec :as s]
    [hundred-pushups.datetime :as dt]))

;;;;;; specs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/def :exr/reps nat-int?)
(s/def :exr/pushup-reps :exr/reps)
(s/def :exr/plank-reps :exr/reps)
(s/def :exr/sets (s/int-in 4 20))
(s/def :exr/ts inst?)

(s/def :exr/circuit
  (s/keys :req [:exr/pushup-reps :exr/plank-reps :exr/ts]))
(s/def :exr/suggested-circuit
  (s/keys :req [:exr/pushup-reps :exr/plank-reps]))
(s/def :exr/test
  (s/keys :req [:exr/pushup-reps :exr/plank-reps :exr/ts]))

(s/def :exr/suggested-circuits (s/keys :req [:exr/sets :exr/suggested-circuit]))

(s/def :exr/action
  (s/or
   :do-circuits :exr/suggested-circuits
   :do-test #{:exr/do-test}))

(s/def :exr/circuits (s/coll-of :exr/circuit))
(s/def :exr/tests (s/coll-of :exr/test))
(s/def :exr/history (s/and (s/keys :req [:exr/circuits
                                         :exr/tests])
                           #(pos?
                              (count (:exr/tests %)))))

;;;;;; private ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def dummy-ts (dt/inst 0))

(defn parse-int [x]
  #?(:clj  (Integer/parseInt x)
     :cljs (js/parseInt x)))

(defn div-ceil [num den]
  (let [q (quot num den)
        r (rem num den)]
    (if (= 0 r)
      q
      (+ 1 q))))

(defn half [num]
  (div-ceil num 2))

(defn map-vals
  "Like `map`, but only for the values of a hash-map that pass the key predicate"
  [f m]
  (into {}
        (for [[k v] m]
          [k (f v)])))

(defn last-days-log [circuits]
  (vec (last (partition-by (comp dt/local-date :exr/ts) circuits))))

(s/fdef day->log
        :args (s/cat :day :exr/action
                     :ts :exr/ts))
(defn day->log [day ts]
  (if (= day :exr/do-test)
    []
    (repeat (:exr/sets day)
            (assoc (:exr/suggested-circuit day)
                   :exr/ts ts))))

(defn but-last-day [circuits]
  (->> circuits
       (partition-by (comp dt/local-date :exr/ts) )
       butlast
       flatten
       vec))

(defn complete? [expected-circuit actual-circuit]
  (and (<= (:exr/pushup-reps expected-circuit)
           (:exr/pushup-reps actual-circuit))
       (<= (:exr/plank-reps expected-circuit)
           (:exr/plank-reps actual-circuit))))

(defn finished-circuits? [expected-day actual-log]
  (let [expected-log (day->log expected-day dummy-ts)]
    (<= (count expected-log) (count actual-log))))

;; TODO - rename??
(defn completed-circuit?
  [expected-day actual-log]
  (let [expected-log (day->log expected-day dummy-ts)]
    (and
     (every? true? (map complete? expected-log actual-log))
     (finished-circuits? expected-day actual-log))))

(defn ts-greater? [ts1 ts2]
  ;; <= works for instants in CLJS, but not CLJ
  (neg? (compare ts1 ts2)))

(declare suggested-day)

(defn analyze-history [history ts]
  "Given a history, adds key/value pairs
   regarding the state of the history"
  (let [{:keys [:exr/circuits
                :exr/tests]} history
        last-circuit (last circuits)
        last-test (last tests)
        ;; TODO - maybe rename these keys? or at least document
        defaults {:fresh-test? false
                  :last-workout-completed? false
                  :done-today? false}]

    (cond-> (merge history defaults)
      (and (nil? last-circuit) last-test)
      (assoc :fresh-test? true
             :last-workout-completed? false)

      (ts-greater? (:exr/ts last-circuit (dt/inst 0)) (:exr/ts last-test))
      (assoc :fresh-test? true)

      (and last-circuit
           (completed-circuit?
            (suggested-day {:exr/tests tests :exr/circuits (but-last-day circuits)})
            circuits))
      (assoc :last-workout-completed? true)

      (and last-circuit
           (dt/later-on-same-day? (:exr/ts last-circuit) ts)
           (finished-circuits?
            (suggested-day {:exr/tests tests :exr/circuits (but-last-day circuits)})
            circuits))
      (assoc :done-today? true)

      (dt/later-on-same-day? (:exr/ts last-test) ts)
      (assoc :done-today? true))))

;;;;;; public ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(s/fdef suggested-day
        :args (s/cat
               :history :exr/history)
        :ret :exr/action)
(defn suggested-day [history]
  (let [{:keys [:exr/circuits
                :exr/tests
                :last-workout-completed?
                :fresh-test?]} (analyze-history history dummy-ts)
        last-circuit (last circuits)
        last-test (last tests)]

    (cond
      fresh-test?
      {:exr/sets 4
       :exr/suggested-circuit (map-vals half (dissoc last-test :exr/ts))}

      last-workout-completed?
      {:exr/sets 4
       :exr/suggested-circuit (map-vals inc (dissoc last-circuit :exr/ts))}

      :else
      :exr/do-test)))

(defn ui-state->path [ui-state]
  (concat
   (for [[k v]  (into [] (:pushup-reps-text ui-state))]
     [[k :exr/pushup-reps] v])
   (for [[k v]  (into [] (:plank-reps-text ui-state))]
     [[k :exr/plank-reps] v] )))

(defn merge-day-changes [day ui-state ts]
  (reduce
   (fn [log [path v]]
     (assoc-in
      log
      path
      (parse-int v)))
   (vec (day->log day ts))
   (ui-state->path ui-state)))

(defn format-whitelist-row [row]
  (str (name(first row)) ": " (clojure.string/join "-" (last row))))

(defn valid-hour-time [input]
  (and (some? input)
  (some? (re-matches #"\d+(am|pm)" input))))
