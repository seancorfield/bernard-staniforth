(ns StanTheBear.mynewapp
  (:require [clojure.string :as s]
            [clojure.repl :refer [doc source]]
             [clojure.math.numeric-tower :as math :refer [expt sqrt]]
            )
  (:gen-class))

(defn sumkey
  [sie sumks vfact cand]
  (if (seq sumks);(not (seq sumks)); (18) [3] 15 (rest sumks)))
    (recur (assoc sie (first sumks) 
                   (cons (first vfact) (sie (first sumks)) ))
           (next sumks) (next vfact) cand)
    (dissoc sie cand))) 

(defn erato
  "is cand key in table: no- add it keyed by cand'sqr, add to primes
  : yes- add cand to each val(factor) if sum is key add val to key's facts
    else enter key sum val(factor) for each sum & val
    remove orig found cand entry"
  ([] (erato {} 2'()  ))
  ([sie cand facts ];prime-nums] 
     (if-let [facts ( sie cand)]
       (erato 
             (sumkey sie (map (partial + cand) facts) facts cand)
              (inc cand) facts)
       (lazy-seq (cons cand 
                      (erato
                        (assoc sie (* cand cand) (list cand))
                        (inc cand)
                        facts)))) ))

(= (drop 1(take 156 primes)) (take 155 (erato))


   
(defn sqr-div 
"inner div trial whilst sqr pos factor is up to number"
  [ns posf facts]
  (if (> (* posf posf) ns)
    (if (not= ns 1) (conj facts ns) facts)
    (if (zero? (mod ns posf)) ; (let [fl (conj facts posf)
            (recur (/ ns posf) posf (conj facts posf))
            (recur ns (+ posf 2) facts))))


   
(defn trial-division-sqr
[num pos-factor flist]
"Return a list of the prime factors for a natural number"
;(let [factors flist]
 (if (zero? (mod num 2))       ; While n still even.
  ;(let [vfacts     (sqr-div (/ num 2) 2 (conj flist 2))]       ; div 2, 2 is factor inc pos-factor
  (recur (/ num 2) 2 (cons 2 flist))
    (sqr-div num 3 flist)))            ; num not even look for 3...



(defn -main
  [& args]
  ())

