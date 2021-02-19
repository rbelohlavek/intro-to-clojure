(ns intro-to-clojure.core-test
  (:require [clojure.test :refer :all]
            [intro-to-clojure.core :refer :all]))

(deftest happy-path
  (testing "some-function works as expected"
    (is (= "a=2, b=3" (some-function 1 2))))

  ;'mocking'
  (testing "mocking some-function details"
    (with-redefs [intro-to-clojure.core/some-inner-detail (fn [a] a)]
      (is (= "a=1, b=2" (some-function 1 2))))))
