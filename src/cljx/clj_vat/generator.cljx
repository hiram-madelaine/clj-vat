(ns clj-vat.generator
  (:require [clj-vat.algo :as a :refer [=10?->0 mod10 zero-mod10 +>9 sum-fn check-fr ->num comp-zero]]
            [clojure.test.check.generators :as gen]))


(def siren-fn
  (sum-fn {:pre reverse
           :weight (cycle [2 1])
           :norm +>9
           :check (comp =10?->0 zero-mod10)
           :value 0}))


(defn siren-generator
  ([]
   (siren-generator (apply str (gen/sample gen/pos-int 8))))
  ([seed]
   (->> seed
        siren-fn
        (str seed))))


(defn siren->vat
  [siren]
  (-> siren
      ->num
      check-fr))

(defn vat-fr-gen
  ([]
   (vat-fr-gen (siren-generator)))
  ([siren]
   (let [siren (siren-generator siren)
         prefix (->> siren
                    siren->vat
                    (comp-zero 2))]
     (str "FR" prefix siren))))