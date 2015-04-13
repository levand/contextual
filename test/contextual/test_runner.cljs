(ns contextual.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [contextual.core-test]))

(enable-console-print!)

(try
  (run-tests
   'contextual.core-test)
  (finally
    (.exit js/phantom)))


