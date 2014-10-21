(ns spaced-repetition.sm5
  "Adapted from org-learn's impl. of the SM5 algo from the SuperMemo spec.")

; if non-nil, always reschedule items, even if retention was "perfect".
(def always-reschedule true)

; controls the rate at which EF is increased or decreased; must be a
; number between 0 and 1 (the greater it is the faster the changes of
; the OF matrix)."
(def learn-fraction 0.5)

(defn initial-optimal-factor [n ef]
  (if (= 1 n) 4 ef))

; the of-matrix has the structure:
; {1 {efA of1A, efB of1B, efC of1C, ..}
;  2 {efA of2A, efB of2B, efC of2C, ..}
;  ..]

(defn get-optimal-factor
  ([n ef] (initial-optimal-factor n ef))
  ([n ef of-matrix] (or (get-in of-matrix [n ef])
                        (initial-optimal-factor n ef))))

(defn inter-repetition-interval
  ([n ef] (inter-repetition-interval [n ef]))
  ([n ef of-matrix] (inter-repetition-interval [n ef of-matrix]))
  ([[n ef :as args]]
     (let [of (apply get-optimal-factor args)]
       (if (= 1 n)
         of
         (* of (inter-repetition-interval (assoc args 0 (dec n))))))))

(defn modify-e-factor [ef quality]
  (if (< ef 1.3)
    1.3
    (let [x5-q (partial * (- 5 quality))]
      (+ ef (- 0.1 (x5-q (+ 0.08 (x5-q 0.02))))))))

(defn modify-of [of q fraction]
  (+ (* (- 1 fraction) of) (* fraction (* of (+ 0.72 (* q 0.07))))))

(defn calculate-new-optimal-factor
  "Takes the following arguments: interval-used, the last interval
  used for the item in question; quality of the repetition response;
  used-of, the optimal factor used in calculation of the last interval
  used for the item in question; old-of, the previous value of the OF
  entry corresponding to the relevant repetition number and the
  E-Factor of the item; fraction, a number belonging to the range
  (0,1) determining the rate of modifications (the greater it is the
  faster the changes of the OF matrix). Returns the newly calculated
  value of the considered entry of the OF matrix. Intermediate values
  involved: mod5, value proposed for the modifier in case of q=5;
  mod2, value proposed for the modifier in case of q=2; modifier,
  number determining how many times the OF value will increase or
  decrease."
  [interval-used quality used-of old-of fraction]
  (let [mod5 (max 1.05 (/ (inc interval-used) interval-used))
        mod2 (max 0.75 (/ (dec interval-used) interval-used))
        modifier (max 0.05 (if (> quality 4)
                             (inc (* (dec mod5) (- quality 4)))
                             (- 1 (* (/ (- 1 mod2) 2) (- 4 quality)))))
        of (let [new-of (* modifier used-of)]
             (if (or (and (> quality 4) (< new-of old-of))
                     (and (< quality 4) (> new-of old-of)))
               old-of
               new-of))]
    (max 1.2 (+ (* of fraction) (* old-of (- 1 fraction))))))

(defn determine-next-interval
  ([quality] (determine-next-interval quality [1 2.5 nil]))
  ([quality [n ef of-matrix :as learn-seq]]
     (assert (> n 0))
     (assert (and (>= quality 0) (<= quality 5)))
     (if (< quality 3)
       {:days-to-next (inter-repetition-interval n ef)
        :learn-seq [(dec n) ef nil]}
       (let [next-ef (modify-e-factor ef quality)
             new-of-matrix
             (assoc-in of-matrix [n next-ef]
                       (modify-of (get-optimal-factor n ef of-matrix)
                                  quality learn-fraction))]
         {:days-to-next (if (and (>= quality 4) (not always-reschedule))
                          0
                          (inter-repetition-interval n next-ef new-of-matrix))
          :learn-seq [(inc n) next-ef new-of-matrix]}))))
