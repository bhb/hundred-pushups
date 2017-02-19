(ns hundred-pushups.datetime
  "Functions to convert between three datetime formats.

  cljs-time/clj-time have internal time formats that
  are useful for doing datetime changes, formatting, and conversions.

  Our app state uses instants (which are js/Date objects in CLJS and
  java.util.Date objects on CLJ). These are easy to readn and write
  from Clojure(script).

  Finally, a React Native component (react-native-datepicker) uses
  moment.js formats."

  (:require [clojure.spec :as s]
            [clojure.string :as str]
            #?@(:clj  [[clj-time.coerce :as time.coerce]
                       [clj-time.core :as time]
                       [clj-time.format :as time.format]]
                :cljs [[cljs-time.coerce :as time.coerce]
                       [cljs-time.core :as time]
                       [cljs-time.format :as time.format]
                       [cljsjs.moment :as moment]])))

(defn past
  "Given an inst and a number of seconds n, returns a inst
   n seconds in the past"
  [inst n]
  (-> (time.coerce/from-date inst)
      (time/minus (time/seconds n))
      (time.coerce/to-date)))

(defn now
  "The current datetime"
  []
  (time.coerce/to-date (time/now)))

(defn inst
  "Given seconds from epoch (or parseable date), returns an inst."
  [second-since-epoch]
  (time.coerce/to-date second-since-epoch))

(defn ct-fmt->moment-fmt
  "Converts a cljs-time format string to a moment.js format str
   See http://momentjs.com/docs/#/parsing/string-format/ for details."
  [ct-fmt]
  (-> (s/assert some? ct-fmt)
      (str/replace "yyyy" "YYYY")
      (str/replace "dd" "DD")))

(defn ct-formatter []
  (:basic-date-time-no-ms time.format/formatters))

#?(:cljs
   (defn moment-fmt []
     (ct-fmt->moment-fmt (:format-str (ct-formatter)))))

#?(:cljs
   (defn moment-str->inst
     "Given a string, returns an inst"
     [moment-str]
     (if-not moment-str
       nil
       (let [time-str (-> moment-str
                          (js/moment (moment-fmt))
                          (.utc)
                          (.format (moment-fmt))
                          (str/replace "+00:00" "Z")
                          (str/replace "'T'" "T"))]
         (time.coerce/to-date (time.format/parse (ct-formatter) time-str))))))

(defn inst->str
  "Given an inst, returns a string representation
  using the default formatter."
  [inst]
  (time.format/unparse (ct-formatter) (time.coerce/from-date inst)))

(defn local-date
  "Given an inst, returns a [local-date local-month local-day]"
  ([inst]
   (local-date inst :default))
  ([inst zone]
   (let [dt (time.coerce/from-date inst)
         local-dt #?(:cljs (time/to-default-time-zone dt)
                     :clj (time/to-time-zone (time.coerce/to-date-time dt)
                                             (if (= :default zone)
                                               (time/default-time-zone)
                                               (time/time-zone-for-id zone))))]
     [(time/year local-dt)
      (time/month local-dt)
      (time/day local-dt)])))

(s/fdef later-on-same-day?
        :args (s/cat :ts1 inst? :ts2 inst?))
(defn later-on-same-day? [ts1 ts2]
  (and (= (local-date ts1)
          (local-date ts2))
       (or (= ts1 ts2)
           (time/before?
            (time.coerce/to-date-time ts1)
            (time.coerce/to-date-time ts2)))))

 (s/fdef start-of-day
         :args (s/cat :ts inst?))
(defn start-of-day [ts]
    (-> ts
        (time.coerce/from-date)
        #?(:clj  (time/with-time-at-start-of-day)
           :cljs (time/at-midnight))
        (time.coerce/to-date)))

(defn greater? [ts1 ts2]
  ;; <= works for instants in CLJS, but not CLJ
  (neg? (compare ts1 ts2)))
