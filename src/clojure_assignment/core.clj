(ns clojure-assignment.core)

(defn lu [i m] (get m i i))

(defn notruth [a] (not= a 'true))

(defn simp [x]
  (if (not (seq? x))
    x
    (let
      [y (filter notruth (distinct x))]
      (cond
        (= 1 (count y)) false
        (some false? y) true
        (not (some seq? y)) y
        :else (if (and (= (count y) 2) (= (count (nth y 1)) 2))
                (nth (nth y 1) 1)
                (if (some
                      #(= % 'true)
                      (map
                        #(if
                           (and
                             (and (seq? %) (= (count %) 2))
                             (some
                               (fn [c] (= c (nth % 1)))
                               y)
                             )
                           true
                           %)
                        y)
                      )
                  true
                  y
                  )
                )
        )
      )
    )
  )

(defn simplify [b]
  (simp
    (if (some seq? b)
      (map
        #(if (seq? %)
           (simplify %)
           %
           )
        b)
      b
      )
    )
  )

(defn help [& aa]
  aa
  )

(defn myor [a]
  (map
    #(if (= % 'or)
       'nand
       (help 'nand %)
       )
    a)
  )

(defn myhelpand [b]
  'nand (map
          #(if (= % 'and)
             'nand
             %
             )
          b)
  )

(defn myand [b]
  (let [y (myhelpand b)]
    (map
      #(if (= % 'filler) y %)
      '(nand filler)
      )
    )
  )

(defn mynot [c]
  (help 'nand (nth c 1))
  )

(defn main [x]
  (cond
    (= (nth x 0) 'or) (main (myor x))
    (= (nth x 0) 'and) (main (myand x))
    (= (nth x 0) 'not) (main (mynot x))
    :else (map
            #(if (seq? %)
               (main %)
               %
               )
            x)
    )
  )

(defn evalexp [l m]
  (simplify
    (main
      (map (fn [i]
             (if (seq? i)
               (evalexp i m)
               (lu i m)))
           l)
      )
    )
  )
