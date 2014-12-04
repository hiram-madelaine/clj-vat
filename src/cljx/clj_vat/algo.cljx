(ns clj-vat.algo)


(defn parse-num
   [s]
#+cljs (js/parseInt s 10)
#+clj (Long/parseLong (str s)))


 (defn i->c
   [i]
#+cljs (.fromCharCode js/String i)
#+clj (str (char i)))


(defn- digits
  [s]
  (vec (map parse-num s)))

(defprotocol Digits
  (->digits [this] "Transforme une chaîne de digits ou une sequence de chaînes de digits en une sequence de digits.
   \"123456\"->[1 2 3 4 5 6]
   [\"1\" \"3\"] -> [1 3]")
  (->num [this] "Convertit une :
   * chaîne de chifres,
   * une séquence de chiffres,
   * une séquence de string
   en un nombre :
   \"2355\" -> 2355
   [2 3 5 5 ] -> 2355
   [\"2\" \"3\" \"5\" \"5\" ] -> 2355"))

#+clj
(extend-protocol Digits

  java.lang.Character
  (->digits [c]
    (->digits (str c)))
  (->num [c]
    (->num (str c)))

  java.lang.String
  (->digits
    [s]
    (digits s))
  (->num
    [s]
    (parse-num s))

  java.lang.Number
  (->digits
    [n]
    (->digits (str n)))
  (->num
    [n]
    (->num (str n)))

  clojure.lang.Sequential
  (->digits
    [xs]
    (vec (map ->num xs)))
  (->num [xs]
    (->num (apply str xs))))



#+cljs
(extend-protocol Digits

  string
  (->digits
    [s]
    (digits s))
  (->num
    [s]
    (parse-num s))

  number
  (->digits
    [n]
    (->digits (str n)))
  (->num
    [n]
    (->num (str n)))

  cljs.core.PersistentVector
  (->digits
    [xs]
    (vec (map ->num xs)))
  (->num [xs]
    (->num (apply str xs)))

  object
  (->digits
    [xs]
    (vec (map ->num xs)))
  (->num [xs]
    (->num (apply str xs)))
  )


(defn split->nums
  "Split une chaîne de chiffres à la position n en deux nombres."
    [n s]
    (map ->num (split-at n s)))

(defn +>9
  "Additionne les chiffres qui composent les nombres supérieurs à 9.
   10 -> 1
   12 -> 3
    9 -> 9"
  [n]
  (if (> n 9)
    (apply + (->digits n))
    n))

(defn p?->r
  "HOF qui génère une fonction qui accepte une valeur v.
  f est une fonction de deux arguments dont un est m.
  Si v est vraie en regard du prédicat #(f % m) alors retourne r."
  [r f m]
  (fn [v]
    (if (f v m) r v)))

(defn p?->0
  "Si une valeur v est vraie pour le prédicat (f v m) alors retourne 0."
  [f m]
  ((partial p?->r 0) f m))


(defn mod-m
  "Génère une fonction de modulo fixe pour un nombre."
  [m]
  (fn [n]
    (mod n m)))

(def mod10 (mod-m 10))
(def mod11 (mod-m 11))

(defn check-vat-gb
  [str]
  (if (re-matches #"[0-9]{9,12}" str)
    (let [ns (->digits str)
         check (->num (subvec ns 7))
         w-sum (reduce + (map * ns (range 8 1 -1)))
         valid? (fn [sum] (->> sum
                               (iterate #(- % 97))
                               (drop-while pos?)
                               first
                               Math/abs
                               (= check)))]
     (some valid? [w-sum (+ w-sum 55)]))
    false))

(def ^:dynamic *debug* false)

(defn debug [x]
  (when *debug*
#+clj (prn x)
#+cljs (.log js/console (pr-str x)))
  x)

(defn sum-fn
  [{:keys [pre weight norm rf init check value]
      :or   {pre    identity
             weight (repeat 1)
             norm   identity
             rf     +
             init   0
             check  identity}}]
  (let [last? #(if value % (butlast %))]
    (fn [s]
      (->> s
           pre
           last?
           ->digits
           (map * weight)
           (map norm)
           (reduce rf init)
           check))))

(defn checksum
  "Permet de valider le checksum d'une suite de chiffres.
   Par défaut, l'élément de contrôle est le dernier élément de la séquence, il ne sert pas au calcul du checksum.
   :options :
   :pre fonction qui permet de manipuler la séquence avant le checksum (substring, reverse...)
   :weight la séquence de pondération.
   :rf la fonction qui sert à réduire la séquence pondérée. Défaut +
   :init Valeur initiale de la somme de la séquence pondérée. Défaut 0
   :check fonction qui détermine le résultat du checksum. Le plus souvent un modulo.
   :value Si une valeur est passé alors le checksum est calculé sur l'ensemble de la séquence."
  ([s]
    (checksum s {}))
  ([s {:keys [pre value regex]
      :or {pre identity
           regex #".+"}
      :as m}]
   (if (re-matches regex s)
    (let [t ((sum-fn m) s)]
      (if value
        (= t value)
        (= (str t)
           (-> s
               pre
               last
               str))))
    false)))


(defn zero-mod
  "HOF - Crée une fonction qui indique pour un diviseur m le nombre à ajouter à une valeur n pour que le modulo de n par m soit zero."
   [m]
   (fn [n] (- m (mod n m))))


(def zero-mod11 (zero-mod 11))
(def zero-mod10 (zero-mod 10))

(defn sub
  ([f]
    (fn [s] (.substring s f)))
  ([f t]
    (fn [s] (.substring s f t))))

(defn luhn
  "Valide un nombre selon l'agorithme de Luhn."
  ([s]
  (luhn s {}))
  ([s opts]
    (let [spec {:pre reverse
                :weight (cycle [1 2])
                :norm +>9
                :check mod10
                :value 0}]
      (checksum s (merge-with comp spec opts)))))

(defn luhn-check-digit
  [s]
  (checksum s {:pre reverse
               :weight (cycle [1 2])
               :norm +>9
               :check zero-mod10}))

(defn check-vat-fr
  "Vérifie la partie numérique d'un numéro de TVA français.
   Check de l'algorithme de Luhn sur le numéro SIREN : les neuf derniers chiffres.
   Check de la clef de TVA : les deux premiers chiffres."
  [s]
  (if (re-matches #"[0-9]{11}" s)
   (let [[clef siren] (split->nums 2 s)]
     (and (luhn (str siren))
          (= clef (-> siren
                      (mod 97)
                      (* 3)
                      (+ 12)
                      (mod 97)))))
   false))

(defn check-vat-dk
  "Vérifie un identifiant de TVA Danois."
  [s]
  (checksum s {:regex #"[1-9]{1}[0-9]{7}"
               :weight [2 7 6 5 4 3 2 1]
               :check mod11
               :value 0}))


(def =10?->0 (p?->0 = 10))

(defn check-vat-de
  [s]
  (let [zero?->10 (p?->r 10 = 0)]
    (checksum s {:regex #"[1-9]{1}[0-9]{8}"
                 :rf (comp mod11 #(* 2 %) zero?->10 mod10 +)
                 :init 10
                 :check (comp =10?->0 #(- 11 %))})))

(defn check-vat-it
  [s]
  (luhn s {:regex #"[0-9]{11}"}))

 (defn check-vat-es
   "Valide un identifiant de TVA Espagnol pour les entreprises commerciales."
   [s]
   (luhn s {:regex #"[0-9A-Z]{1}[0-9]{7}[0-9A-Z]{1}"
            :pre (sub 1 9)}))

 (defn check-vat-se
   [s]
   (luhn s {:regex #"[0-9]{12}"
            :pre (sub 0 10)}))


 (def >9?->0 (p?->0 > 9))

 (defn check-vat-pt
   [s]
   (checksum s {:regex #"[1-9]{1}[0-9]{8}"
                :weight (range 9 1 -1)
                :check (comp >9?->0 zero-mod11)}))

  (defn check-vat-hu
    [s]
    (checksum s {:regex #"[0-9]{8}"
                 :weight [9 7 3 1 9 7 3 1]
                 :check mod10
                 :value 0}))

(defn check-vat-pl
  [s]
  (checksum s {:regex #"^[0-9]{10}$"
               :weight [6 5 7 2 3 4 5 6 7]
               :check  mod11}))

  (defn check-vat-nl
    [s]
    (checksum s {:regex #"^[0-9]{9}B[0-9]{2}$"
                 :pre (sub 0 9)
                 :weight (range 9 1 -1)
                 :check mod11}))



  (defn check-vat-el
    [s]
    (checksum s {:regex #"[0-9]{9}"
                 :weight (iterate #(/ % 2) 256)
                 :check mod11}))

  (defn old->new
    "9 B 12345 N -> 0 12345 9 N"
    [s]
    (if-let [[_ d ns l] (re-matches #"([7-9])[A-Z](\d{5})([A-W])$" s)]
      (str 0 ns d l)
      s))

  (defn check-vat-ie
   [s]
   (let [i->l #(get {0 "W"} % (i->c (+ % 64)))]
     (checksum s {:regex #"^[0-9][0-9A-Z\+\*][0-9]{5}[A-W]$"
                  :pre old->new
                  :weight (range 8 1 -1)
                  :check (comp i->l (mod-m 23))})))


  (defn check-vat-at
    [s]
    (checksum s {:regex #"U[0-9]{8}$"
                 :pre (sub 1)
                 :weight (cycle [1 2])
                 :norm +>9
                 :check #(last (->digits (- 96 %)))}))


  (defn check-vat-is
    [s]
    (checksum s {:weight (range 8 1 -1)
                 :check (comp =10?->0 zero-mod11)}))

  (def =11?->0 (p?->0 = 11))

  (defn check-vat-fi
    [s]
    (checksum s {:regex #"^[0-9]{8}$"
                 :weight [7 9 10 5 8 4 2 1]
                 :check mod11
                 :value 0}))

  (defn check-lv
    [s n]
    (let [m (mod n 11)]
      (cond
        (and (= 4 m) (= 9 (->num (first s)))) (- n 45)
        (= 4 m) 0
        (> m 4) (- 14 m)
        (< m 3) (- 3 m))))

  (defn check-vat-lv
    [s]
    (let [check (partial check-lv s) ]
      (checksum s {:weight [9 1 4 8 3 10 2 5 7 6]
                   :check check})))


(def zero-mod-97 (zero-mod 97))

 (defn check-vat-be
   [s]
   (if-not (re-matches #"0[1-9]{1}[0-9]{8}" s)
     false
    (let [[sum c] (split->nums 8 s)]
      (= c (zero-mod-97 sum)))))

 (defn check-vat-lu
   [s]
   (if (re-matches #"^[0-9]{8}$" s)
    (let [[f c] (split->nums 6 s)]
      (= c (mod f 89)))
    false))

 #_(defn check-tva
   [{:keys [ctry ident]}]
   (let [fn (str "check-vat-" (.toLowerCase ctry))]
     ((intern *ns* (symbol fn)) ident)))

(defn ceil [n m]
  (-> n
      (/ m)
      (Math/ceil)
      (* m)
      int))


(defn nearest-higher-multiple-of
  [n m]
  (let [mod? (mod-m m)]
    (if (zero? (mod? n))
      (+ n m)
      (ceil n m))))



(defn check-vat-cz
  [s]
  (checksum s {:regex #"^[0-8][0-9]{7}$"
               :weight (range 8 1 -1)
               :check #(mod10 (- (nearest-higher-multiple-of % 11) %))}))

(defn check-vat-ee
  [s]
  (checksum s {:regex #"10[0-9]{7}"
               :weight [3 7 1 3 7 1 3 7]
               :check #(- (ceil % 10) %)}))



(def sum-bg (sum-fn {:weight (range 3 11)
                     :check mod11}))

(defn check-vat-bg
  [s]
  (checksum
    s {:regex #"^[0-9]{9}$"
       :weight (range 1 9)
       :check #(let [r1 (mod11 %)]
                (if (= 10 r1)
                  (let [r2 (sum-bg s)]
                    (if (= 10 r2)
                      0
                      r2))
                  r1))}))


(defn check-fn-lt
  [s rg]
  (fn [r]
    (let [r1 (mod11 r)]
      (if-not (= 10 r1)
        r1
        (let [r2 (->> s
                      ->digits
                      butlast
                      (map * rg)
                      (reduce +)
                      mod11)]
          (if (= 10 r2)
            0
            r2))))))

(defn check-vat-lt
  [s]
  (condp = (count s)
        9 (checksum s {:regex #"^[0-9]{9}$"
                       :weight (range 1 9)
                        :check (check-fn-lt s [3 4 5 6 7 8 9 1])})
        12 (checksum s {:regex #"[0-9]{12}"
                        :weight (concat (range 1 10) [1 2])
                        :check  (check-fn-lt s (concat (range 3 10) (range 1 5)))})
      false))


(defn comp-zero
  "Complete s with zeros to attain a lenth of n"
  [n s]
  (let [ss (->digits s)]
    (if (<= 10 (count ss))
      s
      (apply str (comp-zero n (cons "0" ss))))))



(defn check-vat-ro
  [s]
  (checksum s {:regex #"^[0-9]{2,10}$"
               :pre #(comp-zero 10 %)
               :weight [7 5 3 2 1 7 5 3 2]
               :check #(let [r1 (mod11 (* 10 %))]
                        (if (= 10 r1)
                          0
                          r1))}))




(defn check-vat-si
  "Check Slovenia's VAT"
  [s]
  (checksum s {:regex #"^[0-9]{8}$"
               :weight (range 8 1 -1)
               :check #(let [r (zero-mod11 %)]
                        (condp = r
                          10 0
                          11 -1
                          r))}))


(defn check-vat-sk
  "Check Slovakia's VAT"
  [s]
  (if-not (re-matches #"^[0-9]{10}$" s)
    false
    (-> s
        parse-num
        mod11
        zero?)))


(defn check-vat-lv
  "Check Latvia's VAT"
  [s]
  (checksum s {:regex #"^[4-9][0-9]{10}$"
               :weight [9 1 4 8 3 10 2 5 7 6]
               :check #(let [r (- 3 (mod11 %))]
                        (cond
                          (< r -1) (+ r 11)
                          (> r -1) r
                          (= r -1) -1))}))


(def zero-mod37 (zero-mod 37))

(defn check-vat-mt
  "Check Malta's VAT"
  [s]
  (if-not (re-matches #"[0-9]{8}" s)
   false
   (let [[ss c] (split->nums 6 s)]
     (checksum (str ss) {:weight [3 4 6 7 8 9]
                         :value  c
                         :check  #(let [r (zero-mod37 %)]
                                   (if (= 0 r)
                                     37
                                     r))}))))

(def cy-odds
  {0 1
   1 0
   2 5
   3 7
   4 9
   5 13
   6 15
   7 17
   8 19
   9 21})

(def cy-res (zipmap (iterate inc 0) (map char (range 65 (+ 65 26)))))

(defn check-vat-cy
  [s]
  (if-not (re-matches #"^[0-5|9][0-9]{7}[A-Z]$" s)
    false
    (let [mod-26 (mod-m 26)
          [ss [l]] (split-at 8 s)
          ss (->digits ss)
          odds (map cy-odds (take-nth 2 ss))
          evens (take-nth 2 (drop 1 ss))]
      (->> (concat odds evens)
           (reduce +)
           mod-26
           cy-res
           (= l )))))

(defn check-vat-no
  [s]
  (checksum s {:regex #"^[0-9]{9}$"
               :weight [3 2 7 6 5 4 3 2]
               :check #(let [r (zero-mod11 %)]
                        (cond
                          (= r 11) 0
                          (< r 10) r
                          :else -1
                          ))}))



(defn check-vat-che
  [s]
  (checksum s {:regex #"^[0-9]{9}$"
               :weight [5,4,3,2,7,6,5,4]
               :check #(let [r (zero-mod11 %)]
                        (condp = r
                          11 0
                          10 -1
                          r))}))


(defmulti check-ident (fn [s] (.substring s 0 2)))

(defmethod check-ident "FR"
  [s]
  (check-vat-fr (.substring s 2)))

(defmethod check-ident "GB"
  [s]
  (check-vat-gb (.substring s 2)))

(defmethod check-ident "DK"
  [s]
  (check-vat-dk (.substring s 2)))

(defmethod check-ident "ES"
  [s]
  (check-vat-es (.substring s 2)))

(defmethod check-ident "PT"
  [s]
  (check-vat-pt (.substring s 2)))

(defmethod check-ident "PL"
  [s]
  (check-vat-pl (.substring s 2)))

(defmethod check-ident "IT"
  [s]
  (check-vat-it (.substring s 2)))

(defmethod check-ident "DE"
  [s]
  (check-vat-de (.substring s 2)))

(defmethod check-ident "IE"
  [s]
  (check-vat-ie (.substring s 2)))

(defmethod check-ident "NL"
  [s]
  (check-vat-nl (.substring s 2)))

(defmethod check-ident "HU"
  [s]
  (check-vat-hu (.substring s 2)))

(defmethod check-ident "CZ"
  [s]
  (check-vat-cz (.substring s 2)))

(defmethod check-ident "EE"
  [s]
  (check-vat-ee (.substring s 2)))

(defmethod check-ident "BG"
  [s]
  (check-vat-bg (.substring s 2)))

(defmethod check-ident "LT"
  [s]
  (check-vat-lt (.substring s 2)))

(defmethod check-ident "RO"
  [s]
  (check-vat-ro (.substring s 2)))

(defmethod check-ident "LV"
  [s]
  (check-vat-lv (.substring s 2)))

(defmethod check-ident "MT"
  [s]
  (check-vat-mt (.substring s 2)))

(defmethod check-ident "BE"
  [s]
  (check-vat-be (.substring s 2)))

(defmethod check-ident "AT"
  [s]
  (check-vat-at (.substring s 2)))

(defmethod check-ident "SI"
  [s]
  (check-vat-si (.substring s 2)))

(defmethod check-ident "LU"
  [s]
  (check-vat-lu (.substring s 2)))

(defmethod check-ident "SK"
  [s]
  (check-vat-sk (.substring s 2)))

(defmethod check-ident "IS"
  [s]
  (check-vat-is (.substring s 2)))

(defmethod check-ident "FI"
  [s]
  (check-vat-fi (.substring s 2)))


(defmethod check-ident "CY"
  [s]
  (check-vat-cy (.substring s 2)))

#_(defmethod check-ident "NO"
  [s]
  (check-vat-no (.substring s 2)))

#_(defmethod check-ident "CH"
  [s]
  (check-vat-che (.substring s 3)))


(defmethod check-ident :default
  [s]
  true)


(defmulti check-vat :ctry )

 (defmethod check-vat "FR"
  [{:keys [ctry ident]}]
  (check-vat-fr ident))

 (defmethod check-vat "GB"
  [{:keys [ctry ident]}]
  (check-vat-gb ident))

 (defmethod check-vat "DK"
 [{:keys [ctry ident]}]
  (check-vat-dk ident))

 (defmethod check-vat "ES"
  [{:keys [ctry ident]}]
  (check-vat-es ident))

 (defmethod check-vat "PT"
[{:keys [ctry ident]}]
  (check-vat-pt ident))

 (defmethod check-vat "PL"
  [{:keys [ctry ident]}]
  (check-vat-pl ident))

 (defmethod check-vat "IT"
  [{:keys [ctry ident]}]
  (check-vat-it ident))

 (defmethod check-vat "DE"
  [{:keys [ctry ident]}]
  (check-vat-de ident))

 (defmethod check-vat "IE"
  [{:keys [ctry ident]}]
  (check-vat-ie ident))

 (defmethod check-vat "NL"
  [{:keys [ctry ident]}]
  (check-vat-nl ident))

 (defmethod check-vat "HU"
  [{:keys [ctry ident]}]
  (check-vat-hu ident))


