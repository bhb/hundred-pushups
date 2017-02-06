(ns hundred-pushups.datetime-test
  (:require [clojure.test :refer :all]
            [clojure.spec :as s]
            [hundred-pushups.test-helper :refer :all]
            [hundred-pushups.datetime :refer :all]
            [com.gfredericks.test.chuck.clojure-test :refer [checking]]))

(deftest now-test
  (is (inst? (now))))

(deftest inst-test
  (is (inst? (inst 0)))
  (is (inst? (inst 100000000000000)))
  (is (inst? (inst "2016-01-01"))))

(deftest ct-fmt->moment-fmt-test
  (is (= "YYYY" (ct-fmt->moment-fmt "YYYY")))
  (is (= "DD" (ct-fmt->moment-fmt "dd"))))

(deftest inst->str-test
  (is (= "19700101T000000Z" (inst->str (inst 0))))
  (is (= "19700112T134640Z" (inst->str (inst 1000000000)))))

(deftest local-date-test
  (testing "returns date based on timezone"
    (is (= [2016 01 01]
           (local-date #inst "2016-01-02T01:01:01Z")))
    (is (= [2016 01 02]
           (local-date #inst "2016-01-02T12:01:01Z")))
    (is (= [1969 12 31]
           (local-date (inst 0))))
    (is (= [2015 12 31]
           (local-date (inst "2016-01-01T00:00:00Z"))))))

(deftest later-on-same-day?-test
  (is (= true (later-on-same-day? (inst "2016-02-01T00:00:00Z") (inst "2016-02-01T00:00:00Z"))))
  (is (= true (later-on-same-day? (inst "2016-02-01T00:00:00") (inst "2016-02-01T00:00:01"))))
  (is (= false (later-on-same-day? (inst "2016-02-01T00:00:02") (inst "2016-02-01T00:00:01"))))
  (is (= false (later-on-same-day? (inst "2016-02-01T00:00:00") (inst "2016-02-02T00:00:00"))))
  (is (= false (later-on-same-day? (inst "2016-02-01T00:00:00") (inst "2016-01-01T00:00:00")))))
