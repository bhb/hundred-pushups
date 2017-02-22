(ns hundred-pushups.subs
  (:require [re-frame.core :refer [reg-sub]]
            [hundred-pushups.core :as core]
            [hundred-pushups.db :as db]))

(reg-sub
 :stage
 (fn [db _]
   (case (last (:completed-stages db))
     nil :get-started
     :get-started :learn-pushup-form
     :learn-pushup-form :do-pushup-test
     :do-pushup-test :do-plank-test
     :do-plank-test :show-day
     :show-day :show-day)))

(reg-sub
 :ui-state/get
 (fn [db _]
   (:ui-state db)))

(reg-sub
 :schedule/get-whitelist
 (fn [db _]
   (get-in (:schedules db) [:white-list])))

(reg-sub
 :db
 (fn [db _]
   db))

(reg-sub
 :days-exercise
 (fn [db _]
   (core/suggested-day
     {:exr/circuits
      (:circuits db)
      :exr/tests
      (:tests db)}
     (or
      (:simulated-time db)
      (:actual-time db)))))

(reg-sub
 :selected-tab
 (fn [db _]
   (get hundred-pushups.db/tabs (:selected-tab db))))

(reg-sub
 :actual-time
 (fn [db _]
   (:actual-time db)))

(reg-sub
 :simulated-time
 (fn [db _]
   (:simulated-time db)))
