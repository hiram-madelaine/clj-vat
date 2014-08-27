(ns clj-vat.generative
  (:require
   [clj-vat.algo :as vat]
   #+clj[clojure.test.check :as tc]
   #+clj[clojure.test.check.generators :as gen]
   #+clj[clojure.test.check.properties :as prop]
   #+cljs[cemerick.double-check.core :as sc]
   #+cljs[cemerick.double-check.generators :as gen]
   #+cljs[cemerick.double-check.properties :as prop :include-macros true]))




(def idempotent-digits-nums
  (prop/for-all [v gen/pos-int]
                (= v (vat/->num  (vat/->digits v)))))

(tc/quick-check 10000 idempotent-digits-nums)



(def idempotent-digits-strings
  (prop/for-all [v gen/pos-int]
                (let [s (str v)]
                 (= v (vat/->num  (vat/->digits s))))))


(tc/quick-check 10000 idempotent-digits-strings)
