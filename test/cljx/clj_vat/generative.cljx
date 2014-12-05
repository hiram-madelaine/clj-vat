(ns clj-vat.generative
  (:require
   [clj-vat.algo :as vat]
   [clojure.test.check.generators :as gen]
   [clojure.test.check :as tc]
   [clojure.test.check.properties :as prop :include-macros true]))



(def idempotent-digits-nums
  (prop/for-all [v gen/pos-int]
                (= v (vat/->num  (vat/->digits v)))))

(tc/quick-check 10000 idempotent-digits-nums)



(def idempotent-digits-strings
  (prop/for-all [v gen/pos-int]
                (let [s (str v)]
                 (= v (vat/->num  (vat/->digits s))))))


(tc/quick-check 10000 idempotent-digits-strings)
