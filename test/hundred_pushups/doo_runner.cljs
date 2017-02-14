(ns hundred-pushups.doo-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [hundred-pushups.datetime-test]
            [hundred-pushups.db-test]
            [hundred-pushups.core-test]))

(doo-tests
 'hundred-pushups.core-test
 'hundred-pushups.datetime-test
 'hundred-pushups.db-test)
