(ns technical-indicators.core
  (:import (java.util Date)))

;; ---------------------------------------------------------------------------
;; rate conversion
(defn ->percentage [[d v]]
  [d (* 100 (dec v))])

(defn percentage-> [[d p]]
  [d (inc (/ p 100))])

(defn monthly->daily [v]
  (Math/pow v (/ 1 21)))

(defn yearly->monthly [v]
  (Math/pow v (/ 1 12)))

(defn yearly->daily [v]
  (Math/pow v (/ 1 365)))


;; --------------------------------------------------------------------------
;; Return
(defn nominal-return [p1 p2]
  (inc (/ (- p2 p1) p1)))


(defn percentage-return [p1 p2]
  (* p1 p2))


(defn daily-return [return-calculation prices]
  (map (fn [[d1 v1] [d2 v2]]
         [d2 (return-calculation v1 v2)])
       prices
       (rest prices)))


(defn by-month [d]
  (str (.getYear d) "-" (.getMonth d)))


(defn by-year [d]
  (str (.getYear d)))

(defn returns-by [group-fn daily-returns]
  (->> daily-returns
       (group-by group-fn)
       (map (fn [[k v]]
              [(ffirst v)
               (reduce * 1 (map second v))]))))


(defn cumulative-return [returns start-date]
  (->> returns
       (filter (fn [[d v]]
                 (>= d start-date)))
       (reduce (fn [acc [d v]]
                 (let [[d1 v1] (first acc)]
                   (cons [d (* v v1)] acc)))
               [[start-date 1]])
       (reverse)))


(defn relative-return [benchmark asset]
  (map (fn [b a]
         [(first a)
          (* 100 (/ (second (->percentage a))
                    (second (->percentage b))))])
       benchmark
       asset))


(defn returns-by-month [asset]
  (returns-by #(by-month (first %))
              asset))

;; ----------------------------------------------------------------------------
;; Fluctuation

(defn daily-fluctuation [prices]
  (daily-return nominal-return prices))

;; ----------------------------------------------------------------------------
;; Volatility

(defn std-dev [samples]
  (let [n (count samples)
        mean (/ (reduce + samples) n)
        intermediate (map #(Math/pow (- %1 mean) 2) samples)]
    (Math/sqrt (/ (reduce + intermediate) n))))


(def annual-factor (Math/sqrt 252))
(def monthly-factor (* annual-factor (Math/sqrt (/ 1 12))))

(def three-month-window 62)

(defn volatility [prices window factor]
  (->> prices
       (map second)
       (partition window 1)
       (map std-dev)
       (map #(* % factor))
       (map vector (drop window
                         (map first prices)))))


;;-----------------------------------------------------------------------------
;; Series

(defn historical-serie [asset]
  (:prices asset))


(defn daily-fluctuation-serie [asset]
  (map ->percentage
       (daily-fluctuation (:prices asset))))


(defn annual-volatility-serie [asset]
  (volatility (daily-fluctuation-serie asset)
              three-month-window
              annual-factor))


(defn cumulative-return-serie [asset]
  (map ->percentage
       (cumulative-return (:daily-returns asset) (Date. 2013 0 1))))


(defn relative-return-serie [benchmark asset]
  (relative-return (sort (returns-by-month (filter (fn [[d v]]
                                                     (>= d (ffirst (:daily-returns asset))))
                                                   (:daily-returns benchmark))))
                   (sort (returns-by-month (:daily-returns asset)))))