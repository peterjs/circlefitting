;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(+ 1 1)

(. Math sqrt 4)

(defn ad [a b]
  (+ a b))

(ad 3 1)

(defn center [circ]
  )

(:x {:x 4})

(map (fn [a b]
       {(key a) (- (val a) (val b))})
     {:x 3 :y 4} {:x 6 :y 8})

(defn mapmaps [f mapa mapb]
  (merge-with f mapa mapb))

;(defn distvects [circa circb]
;  (mapmaps - circb circa))



(defn distvects [circa circb]
  (assoc-in (merge-with - circb circa) [:r] 0))

(dist2 {:x 5 :y 7} {:x 4 :y 8})

(Math/pow 2 3)

(defn lengthvect [vect]
  (Math/sqrt (reduce +
                     (map (fn [a] (Math/pow (val a) 2)) vect))))

(lengthvect {:x 2 :y 2})

(distvects {:x 1 :y 0 :r 3} {:x 2 :y 2 :r 14})

(lengthvect (distvects {:x 0 :y 0} {:x 2 :y 2}))

; the dist between two centers has to be > 2*r
;(defn )

(type {:a "a"})
(type (distvects {:x 0 :y 0} {:x 2 :y 2}))
(type (seq (distvects {:x 0 :y 0} {:x 2 :y 2})))
(seq (distvects {:x 0 :y 0} {:x 2 :y 2}))

(select-keys {:x 5 :y 6 :r 20} [:x :y])


(defn overlap? [circa circb]
  (< (lengthvect (distvects circa circb)) (+ (circa :r) (circb :r))))

(lengthvect (distvects {:x 0 :y 0 :r 2} {:x 1 :y 1 :r 2}))

(overlap? {:x 0 :y 0 :r 2} {:x 1 :y 1 :r 2})
(overlap? {:x 0 :y 0 :r 1} {:x 2 :y 0 :r 1})

(eval (do (+ 34 4)))

(defn permutations [permslist length sofar]
  (if (not (= length 0))
    (concat permslist
      (permutations permslist (- length 1) (conj sofar 0))
      (permutations permslist (- length 1) (conj sofar 1)))
    [sofar]))

(defn cartesian-product
  "All the ways to take one item from each sequence"
  [& seqs]
  (let [v-original-seqs (vec seqs)
        step
        (fn step [v-seqs]
          (let [increment
                (fn [v-seqs]
                  (loop [i (dec (count v-seqs)), v-seqs v-seqs]
                    (if (= i -1) nil
                        (if-let [rst (next (v-seqs i))]
                          (assoc v-seqs i rst)
                          (recur (dec i) (assoc v-seqs i (v-original-seqs i)))))))]
            (when v-seqs
               (cons (map first v-seqs)
                     (lazy-seq (step (increment v-seqs)))))))]
    (when (every? seq seqs)
      (lazy-seq (step v-original-seqs)))))

(defn cartesian-product2
  "All the ways to take one item from each sequence"
  [& seqs]
  ;(print seqs)
  ;(print "\n")
  ;(print (vec seqs))
  ;(seq x) is the recommended idiom for testing if a collection is not empty
  ;(when (every? seq seqs)
  ;  (print (type (first seqs))))
  )

 (cons [3 7 9 12] [4])

 ([[2 3 5] [3 5 6 2]] 1)
 ([[2 3 5] [3 5 6 2]] 1)
 (next [3])
 (rest [3 4 5])

(defn selections
  "All the ways of taking n (possibly the same) elements from the sequence of items"
  [items n]
  (apply cartesian-product (take n (repeat items))))

;(repeat [10 23 56])

(take 2 (repeat [0 1]))

(count (take 3 (repeat [10 23 56])))

(cartesian-product2 [0 1] [0 1] [0 1])
(cartesian-product [0 1] [0 1] [0 1])
(cartesian-product [0 1] [0 1] [0 1] [])
(selections [0 1] 2)

;(println "he")

(permutations [] 3 [])
(count (permutations [] 10 []))

(concat [] [[4 24 6]] [[5 9]])

(vector (vector [] [1 2 3]) [3 4 5])

(concat [1] (concat [1] [3]))

(defn generate-sink [sizex sizey step presentpos]
  (for [x (range 0 sizex step)
        y (range 0 sizey step)
        :let [presentpos2 (next presentpos)
              ispresent (first presentpos2)]
        ]
    (println "x: " x "y: " y "ispresent: " ispresent)))

(generate-sink 3 3 1 [7 8 9 10 11])

(defn generate-sink2 [sizex sizey step presentpos]
  (filter #(not (nil? %))
   (map (fn [[x y] present]
         ;(println "x: " x "y: " y "present: " present)
         (if (= present 1)
           {:x x :y y :r 2}
           nil)
         )
       (for [x (range 0 sizex step)
             y (range 0 sizey step)]
         [x y])
       presentpos
  )))

(generate-sink2 2 2 1 [1 1 1 1 1])

(defn all-sinks []
  (let [sizex 5
        sizey 5
        step 1
        allpresets (selections [0 1] (* sizex sizey (/ 1 step)))]
    (map #(generate-sink2 sizex sizey step %) allpresets)))

(all-sinks)

; if disc=0 1 solution...tangent
; disc>0 2 solutions
; disc<0 0 solutions

(defn disc [a b c]
  (Math/sqrt (- (Math/pow b 2) (* 4 a c))))

(disc 6 56 6)

(defn quadeq [a b c]
  (let [d (disc a b c)]
    (map #(/ (% (- b) d) (* 2 a)) [+ -])))

(quadeq 1 -3 -4)
(quadeq 1 0 -4)

(def select-values (comp vals select-keys))

(select-keys {:b 4 :c 6 :d 8} [:b :c])
(select-values {:b 4 :c 6 :d 8} [:b :c])

(defn t1 [f]
  (f 4 5))
(- 5)

(t1 +)

; edges!
;; (defn line-circle-intersect [line circle]
;;   (let [m (ax/bx)
;;         a (+ (Math/pow m 2) 1)
;;         b (* 2 (- (* m k) (* m q) (p)))
;;         c (+ (Math/pow q 2) (- (Math/pow r 2) 2) (Math/pow p 2) (- (* 2 k q)) (Math/pow k 2))]))

(defn intersets-edges? [sinkx sinky cirlce]
  )

(map - [1 2])

(get-in {:min {:x 5 :y 7} :max {:x 61 :y 9}} [:max :y])
(({:min {:x 5 :y 7} :max {:x 61 :y 9}} :max) :y)

(defn circle-inside-of-aabb? [aabb circle]
  (and (<= (get-in aabb [:min :x]) (- (circle :x) (circle :r)))
       (<= (get-in aabb [:min :y]) (- (circle :y) (circle :r)))
       (>= (get-in aabb [:max :x]) (+ (circle :x) (circle :r)))
       (>= (get-in aabb [:max :y]) (+ (circle :y) (circle :r)))))

(circle-inside-of-aabb? {:x 3 :y 2 :r 2} {:min {:x 0 :y 0} :max {:x 4.9 :y 9}})

(defn circles-overlap? [circles]
  (loop [circle (first circles)
         rst (next circles)]
    (if (= (count rst) 0)
      false
      (if (every? #(not (overlap? circle %)) rst)
        (recur (first rst) (next rst))
        true))))

(defn valid-sink? [circles sink-aabb]
  (and (every? (partial circle-inside-of-aabb? sink-aabb) circles)
       (not (circles-overlap? circles))))

(circles-overlap? '({:x 0 :y 0 :r 5} {:x 1 :y 1 :r 2} {:x 6 :y 8 :r 5}))

(valid-sink? '({:x 0 :y 0 :r 5} {:x 1 :y 1 :r 2} {:x 6 :y 8 :r 5}) {:min {:x -5 :y -5} :max {:x 11 :y 13}})

(valid-sink? '({:x 0 :y 0 :r 5} {:x 6 :y 8 :r 5}) {:min {:x -5 :y -5} :max {:x 11 :y 13}})

(all-sinks)

;(filter #(first %) (map vector (map #(valid-sink? % {:min {:x 0 :y 0} :max {:x 10 :y 10}}) (all-sinks)) (all-sinks)))

;GENERATE ALL SINKS AND VALIDATE THEM
;VISUALIZE (incanter)

(range 0 2 0.5)

(for [x (range 0 3 1)
      y (range 0 3 1)]
  [x y])

(range 0 10 2)

(defn fill-sink [])

;all with all
(defn check [checkfn a b]
  (checkfn a b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; genetic                  ;
; individual = sink + aabb ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn score-ind [individual]
  (let [circles (individual :circles)
        aabb (individual :aabb)]
  (if (valid-sink? circles aabb)
    (count circles)
    0)))

(score-ind {:circles [{:x 0 :y 0 :r 5} {:x 1 :y 1 :r 2} {:x 6 :y 8 :r 5}]
            :aabb {:min {:x -5 :y -5} :max {:x 11 :y 13}}})

(score-ind {:circles [{:x 0 :y 0 :r 5} {:x 6 :y 8 :r 5}]
            :aabb {:min {:x -5 :y -5} :max {:x 11 :y 13}}})

(defn score-gen [generation]
  (map score generation))

(score-gen [{:circles [{:x 0 :y 0 :r 5} {:x 1 :y 1 :r 2} {:x 6 :y 8 :r 5}]
            :aabb {:min {:x -5 :y -5} :max {:x 11 :y 13}}}
            {:circles [{:x 0 :y 0 :r 5} {:x 6 :y 8 :r 5}]
            :aabb {:min {:x -5 :y -5} :max {:x 11 :y 13}}}])

;do it proportionally to the size of the sink
(defn move-circle [circle]
  (reduce #(update-in % [%2] + (- (rand 2) 1))
          circle
          [:x :y]))

(move-circle {:x 5 :y 5 :r 2})

(defn add-circle [circles]
            )

(defn remove-circle [circles]
  )

(defn mutate [individual mutation]
  (rand))

(rand)

(defn next-generation [current-gen])

(defn selection)

(defn evolution [generation]
  (loop [gen generation
         count-gen 0]
    (if (< count-gen 10)
      (recur gen (inc count-gen))
      count-gen)))

(evolution nil)
