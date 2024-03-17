(ns li-cli-completions.core-test
  (:require [clojure.test :refer [deftest is]]
            [li-cli-completions.core :as completions]))

(defn nc
  "Normalize completions"
  [completions]
  (->> completions
       (map #(if (map? %) % {:candidate %}))
       (sort-by :candidate)))

(def nested1
  {:name "script"
   :flags ["--root-flag <flagx>" {:completions {:flagx #{"foo" "bar" "baz"}}}]
   :commands ["foo <x>" {:command (fn [opts])
                         :completions {:x (fn []
                                            ["xcomp" "ycomp"])}
                         :flags ["--foo-flag <foo-flagx>" {}
                                 "--foo-noarg" {}]}]})

(deftest get-completions-test
  (is (= ["--help" "--root-flag" "--foo-flag" "--foo-noarg"]
         (completions/get-completions nested1 ["foo"] "--")))
  (is (= ["--help" "--root-flag"]
         (completions/get-completions nested1 [] "--")))
  (is (= ["xcomp" "ycomp"]
         (completions/get-completions nested1 ["foo"] "")))
  (is (= ["xcomp" "ycomp"]
         (completions/get-completions nested1 ["foo" "--foo-noarg"] ""))))
