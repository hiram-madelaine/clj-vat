(ns clj-vat.algo)


(defn parse-num
   [s]
#+cljs (js/parseInt s)
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
   * une séquence de strin
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
  (let [ns (->digits str)
        check (->num (subvec ns 7))
        w-sum (reduce + (map * ns (range 8 1 -1)))
        valid? (fn [sum] (->> sum
                        (iterate #(- % 97))
                        (drop-while pos?)
                        first
                        Math/abs
                        (= check)))]
    (some valid? [w-sum (+ w-sum 55)])))

(def ^:dynamic *debug* false)

(defn debug [x]
  (when *debug*
#+clj (prn x)
#+cljs (.log js/console (pr-str x)))
  x)



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
  ([s {:keys [pre weight norm rf init check value]
      :or {pre identity
           weight (repeat 1)
           norm identity
           rf +
           init 0
           check identity}}]
  (let [last? #(if value % (butlast %))
        ss (pre s) _ (debug ss)
        t (->> ss
            last?
            ->digits
            (map * weight)
            (map norm)
            (reduce rf init)
            check)]
    (if value
      (= t value)
      (= (str t) (str (last ss)))))))


(defn zero-mod
  "HOF - Crée une fonction qui indique pour un diviseur m le nombre à ajouter à une valeur n pour que le modulo de n par m soit zero."
   [m]
   (fn [n] (- m (mod n m))))

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
               :check (zero-mod 10)}))

(defn check-vat-fr
  "Vérifie la partie numérique d'un numéro de TVA français.
   Check de l'algorithme de Luhn sur le numéro SIREN : les neuf derniers chiffres.
   Check de la clef de TVA : les deux premiers chiffres."
  [s]
  (let [[clef siren] (split->nums 2 s)]
    (and (luhn (str siren))
         (= clef (-> siren
                   (mod 97)
                   (* 3)
                   (+ 12)
                   (mod 97))))))

(defn check-vat-dk
  "Vérifie un identifiant de TVA Danois."
  [s]
  (checksum s {:weight [2 7 6 5 4 3 2 1]
               :check mod11
               :value 0}))


(def =10?->0 (p?->0 = 10))

(defn check-vat-de
  [s]
  (let [zero?->10 (p?->r 10 = 0)]
    (checksum s {:rf (comp mod11 #(* 2 %) zero?->10 mod10 +)
                 :init 10
                 :check (comp =10?->0 #(- 11 %))})))

 (defn check-vat-it
   [s]
   (luhn s))

 (defn check-vat-es
   "Valide un identifiant de TVA Espagnol pour les entreprises commerciales."
   [s]
   (luhn s {:pre (sub 1 9)}))

 (defn check-vat-se
   [s]
   (luhn s {:pre (sub 0 10)}))


 (def >9?->0 (p?->0 > 9))

 (defn check-vat-pt
   [s]
   (checksum s {:weight (range 9 1 -1)
                :check (comp >9?->0 (zero-mod 11))}))

  (defn check-vat-hu
    [s]
    (checksum s {:weight [9 7 3 1 9 7 3 1]
                 :check mod10
                 :value 0}))

  (defn check-vat-pl
    [s]
	  (checksum s {:weight [6 5 7 2 3 4 5 6 7]
                :check mod11}))

  (defn check-vat-nl
    [s]
    (checksum s {:pre (sub 0 9)
                 :weight (range 9 1 -1)
                 :check mod11}))

  (defn check-vat-el
    [s]
    (checksum s {:weight (iterate #(/ % 2) 256)
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
     (checksum s {:pre old->new
                  :weight (range 8 1 -1)
                  :check (comp i->l (mod-m 23))})))


  (defn check-vat-at
    [s]
    (checksum s {:pre (sub 1)
                 :weight (cycle [1 2])
                 :norm +>9
                 :check #(last (->digits (- 96 %)))}))


  (defn check-vat-is
    [s]
    (checksum s {:weight (range 8 1 -1)
                 :check (comp =10?->0 (zero-mod 11))}))

  (def =11?->0 (p?->0 = 11))

  (defn check-vat-fi
    [s]
    (checksum s {:weight [7 9 10 5 8 4 2 1]
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


 (defn check-vat-be
   [s]
   (let [[sum c] (split->nums 7 s)]
     (= c
        ((zero-mod 97) sum))))

 (defn check-vat-lu
   [s]
   (let [[f c] (split->nums 6 s)]
     (= c
       (mod f 89))))

 #_(defn check-tva
   [{:keys [ctry ident]}]
   (let [fn (str "check-vat-" (.toLowerCase ctry))]
     ((intern *ns* (symbol fn)) ident)))

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


