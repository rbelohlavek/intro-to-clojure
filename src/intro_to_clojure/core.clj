(ns intro-to-clojure.core)

(declare some-inner-detail)

(defn some-function [a b]
  (str "a=" (some-inner-detail a) ", b=" (some-inner-detail b)))

(defn- some-inner-detail [thing]
  (inc thing))
