(ns hundred-pushups.db-test
  (:require [clojure.test :refer [deftest is]]
            [clojure.spec :as s]
            [hundred-pushups.test-helper :refer []]
            [hundred-pushups.db :as db :refer [default-db]]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]))

(deftest default-db-test
  (is (= "Success!\n"
         (s/explain-str ::db/app-db default-db))))
