(ns spark-sql
  (:require [sparkling.conf :as conf])
  (:require [sparkling.core :as spark])
  (:require [clojure.data.csv :as csv]))

(defonce sc (-> (conf/spark-conf)
                (conf/app-name "SQL vs Spark")
                (conf/master "local")
                (conf/set "spark.driver.allowMultipleContexts" "true")
                (spark/spark-context)))

(def iowa-liquor-sales "hdfs://localhost:9000/data/iowa_liquor_sales.csv")

(def name->index
  {"DATE"                {:index 0 :type :date :parse-fn #(.parse (java.text.SimpleDateFormat. "yyyy-MM-dd") %)}
   "CONVENIENCE STORE"   {:index 1 :type :string :parse-fn identity}
   "STORE"               {:index 2 :type :string :parse-fn identity}
   "NAME"                {:index 3 :type :string :parse-fn identity}
   "ADDRESS"             {:index 4 :type :string :parse-fn identity}
   "CITY"                {:index 5 :type :string :parse-fn identity}
   "ZIPCODE"             {:index 6 :type :string :parse-fn identity}
   "STORE LOCATION"      {:index 7 :type :string :parse-fn identity}
   "COUNTY NUMBER"       {:index 8 :type :integer :parse-fn #(Integer/parseInt %)}
   "COUNTY"              {:index 9 :type :string :parse-fn identity}
   "CATEGORY"            {:index 10 :type :string :parse-fn identity}
   "CATEGORY NAME"       {:index 11 :type :string :parse-fn identity}
   "VENDOR NO"           {:index 12 :type :string :parse-fn identity}
   "VENDOR"              {:index 13 :type :string :parse-fn identity}
   "ITEM"                {:index 14 :type :string :parse-fn identity}
   "DESCRIPTION"         {:index 15 :type :string :parse-fn identity}
   "PACK"                {:index 16 :type :string :parse-fn #(Double/parseDouble %)}
   "LITER SIZE"          {:index 17 :type :string :parse-fn #(Double/parseDouble %)}
   "STATE BTL COST"      {:index 18 :type :string :parse-fn #(Double/parseDouble %)}
   "BTL PRICE"           {:index 19 :type :string :parse-fn #(Double/parseDouble %)}
   "BOTTLE QTY"          {:index 20 :type :string :parse-fn #(Double/parseDouble %)}
   "TOTAL"               {:index 21 :type :string :parse-fn #(Double/parseDouble %)}
   })


;; utility

(def ppr clojure.pprint/pprint)

(defn table [data]
  (let [table (->> data
                   (mapv
                    (fn [v]
                      (->> v
                           (interpose "\t")
                           (apply str))))
                   (interpose "\n")
                   (apply str))]
    (spit
     "/Users/mkoz/temp/table.txt"
     table)
    (println table)))


(defn csv-read-line [line]
  (first (csv/read-csv line)))
  
(defn select-* []
  (->> (spark/text-file sc iowa-liquor-sales)
       (spark/map csv-read-line)
       (spark/collect)))

(defn select-count []
  (->> (spark/text-file sc iowa-liquor-sales)
       (spark/count)))

(defn select-top-5 []
  (->> (spark/text-file sc iowa-liquor-sales)
       (spark/map csv-read-line)
       (spark/take 5)))

(defn- columns [columns rdd]
  (spark/map 
   (fn [row]
     (let [indices (->> columns
                        (map #(get-in name->index [% :index]))
                        (set))]
       (->> row
            (keep-indexed (fn [i v] (if (indices i) v)))
            (map-indexed (fn [idx elem]
                           (if (clojure.string/blank? elem)
                             nil
                             (let [column (nth columns idx)
                                   parsefn (get-in name->index [column :parse-fn])]
                               (parsefn elem)))))
            (vec))))
   rdd))

(defn- read-csv [sc file]
  (->> (spark/text-file sc file)
       (spark/map csv-read-line)))

(defn select-fields []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["DATE" "CATEGORY NAME" "DESCRIPTION" "TOTAL"])
       (spark/take 5)))

(defn select-fields-offset []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["DATE" "CATEGORY NAME" "DESCRIPTION" "TOTAL"])
       (spark/repartition 1) ;; ouch
       (spark/map-partition-with-index
        (fn [idx iter]
          (when (zero? idx)
            (dotimes [_ 1e6] (.next iter)))
          iter))
       (spark/take 5)))


(defn select-sample []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["DATE" "CATEGORY NAME" "DESCRIPTION" "TOTAL"])
       (spark/sample true 5E-6 (System/currentTimeMillis))
       (spark/take 10)))

(defn distinct-categories []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CATEGORY NAME"])
       (spark/distinct)
       (spark/collect)))

(defn select-filter []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CITY" "CATEGORY NAME" "DESCRIPTION" "TOTAL"])
       (spark/filter (fn [[_ cat _ _]] (re-find #"(?i)(bourbon|whisk)" cat)))
       (spark/take 10)))

(defn select-filter-2 []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CITY" "CATEGORY NAME" "DESCRIPTION" "TOTAL"])
       (spark/filter (fn [[_ cat _ _]] (and cat (re-find #"(?i)(bourbon|whisk)" cat))))
       (spark/filter (fn [[_ _ _ total]] (and total (> total 100))))
       (spark/take 10)))


(defn distinct-categories []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CATEGORY NAME"])
       (spark/distinct)
       (spark/collect)))



(defn order-total []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CATEGORY NAME" "DESCRIPTION" "TOTAL" "BOTTLE QTY"])
       (spark/filter (fn [[cat _ _ _]] (and cat (re-find #"(?i)(bourbon|whisk)" cat))))
       (spark/map-to-pair
        (fn [[_ _ total _ :as row]]
          (spark/tuple total row)))
       (spark/sort-by-key < false)
       (spark/values)
       (spark/take 1)))


(defn cheapest-bourbon []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CATEGORY NAME" "DESCRIPTION" "LITER SIZE" "BTL PRICE"])
       (spark/filter (fn [[cat _ _ _]] (and cat (re-find #"(?i)bourbon" cat))))
       (spark/filter (fn [[_ _ liter _]] (and liter (pos? liter))))
       (spark/map (fn [[_ _ liter price :as row]] (conj row (/ (* 1000 price) liter))))
       (spark/map-to-pair (fn [[_ desc _ _ _ :as row]] (spark/tuple desc row)))
       (spark/combine-by-key
        (fn [row] (conj row 1))
        (fn [acc row] (-> acc
                          (update-in [4] + (nth row 4))
                          (update-in [5] inc)))
        (fn [acc1 acc2] (-> acc1
                            (update-in [4] + (nth acc2 4))
                            (update-in [5] + (nth acc2 5)))))
       (spark/map-values
        (fn [[cat desc price liter total-price-per-liter cnt]]
          [cat desc price liter (/ total-price-per-liter cnt)]))
       (spark/values)
       (spark/map-to-pair
        (fn [[_ _ _ _ avg-price-per-liter :as row]]
          (spark/tuple avg-price-per-liter row)))
       (spark/sort-by-key < true)
       (spark/values)
       (spark/take 1)))

(defn score []
  (->> (read-csv sc iowa-liquor-sales)
       (columns ["CATEGORY NAME" "DESCRIPTION" "LITER SIZE" "BTL PRICE" "BOTTLE QTY"])
       (spark/filter (fn [[cat _ _ _]] (and cat (re-find #"(?i)bourbon" cat))))
       (spark/filter (fn [[_ _ liter _]] (and liter (pos? liter))))
       (spark/map (fn [[_ _ liter price _ :as row]] (conj row (/ (* 1000 price) liter))))
       (spark/map (fn [[_ _ _ price qty _ :as row]] (conj row (* price qty))))
       (spark/map-to-pair (fn [[_ desc _ _ _ _ _ :as row]] (spark/tuple desc row)))
       (spark/combine-by-key
        (fn [row] (conj row 1))
        (fn [acc row] (-> acc
                          (update-in [5] + (nth row 5)) ;; sum of price per liters
                          (update-in [6] + (nth row 6)) ;; sum of liters sold
                          (update-in [7] inc)           ;; count 
                          ))
        (fn [acc1 acc2] (-> acc1
                            (update-in [5] + (nth acc2 5))
                            (update-in [6] + (nth acc2 6))
                            (update-in [7] + (nth acc2 7)))))
       (spark/map-values
        (fn [[cat desc price liter qty total-ppl liters-sold cnt]]
          [desc (/ liters-sold (/ total-ppl cnt))]))
       (spark/values)
       (spark/map-to-pair
        (fn [[desc score :as row]]
          (spark/tuple score row)))
       (spark/sort-by-key < false)
       (spark/values)
       (spark/take 3)))
