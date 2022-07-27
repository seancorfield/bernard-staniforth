(ns prime-sieve
  (:gen-class))

(defn add-candidates
  [sieve candidates factors candidate]
  (if (seq candidates)
    (recur (assoc sieve
                  (first candidates)
                  (cons (first factors)
                        (sieve (first candidates))))
           (next candidates)
           (next factors)
           candidate)
    (dissoc sieve candidate)))

(defn erato
  "is candidate key in table
  : no- add it keyed by candidate'sqr, add to primes
  : yes- add candidate to each val(factor) if sum is key add val to key's facts
    else enter key sum val(factor) for each sum & val
    remove orig found candidate entry"
  ([] (erato {} 2 '()))
  ([sieve candidate factors]
   (if-let [candidate-factors (sieve candidate)]
     (let [sieve' (add-candidates sieve (map (partial + candidate) candidate-factors) candidate-factors candidate)]
       (erato sieve' (inc candidate) candidate-factors))
     (lazy-seq (cons candidate
                     (erato (assoc sieve (* candidate candidate) (list candidate))
                            (inc candidate)
                            factors))))))

(comment
  (take 10 (erato))
  (take 10 (drop 1500 (erato)))
  (def primes []) ; assume some fixed data here I don't have
  (= (drop 1 (take 156 primes)) (take 155 (erato)))
  )

(defn erato-filter
  ([] (erato-filter (iterate inc 2)))
  ([unfiltered]
   (let [prime (first unfiltered)]
     (cons prime
           (lazy-seq
            (erato-filter (remove #(zero? (mod % prime)) (rest unfiltered))))))))

(comment
  (take 10 (erato-filter))
  (take 10 (drop 150 (erato-filter)))
  (take 10 (drop 1500 (erato-filter)))
  )



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
