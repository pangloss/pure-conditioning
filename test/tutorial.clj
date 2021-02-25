(ns tutorial
  (:require [conditions :refer :all]
            [clojure.java.io :refer [reader]]
            [clojure.string :as str]))

(defn validate-url
  "The URL of the page; should start with http:// or https://."
  [string]
  (when-not (re-matches #"^https?://.*" string)
    (condition :url-invalid (restarts {:url string})
               (error "URL invalid"))))

(defn validate-rating
  "String should contain an integer between 1 and 5, inclusive."
  [string]
  (let [rating (read-string string)]
    (when-not (and (int? rating) (<= 1 rating 5))
      (condition :invalid-rating (restarts {:rating rating})
                 (error "Rating is not an integer in range")))))

(defn validate-visitors
  "The number of visitors to the page; string should contain an
  integer more than or equal to zero."
  [string]
  (let [visitors (read-string string)]
    (when-not (nat-int? visitors)
      (condition :invalid-visitors (restarts {:visitors visitors})
                 (error "Number of visitors invalid")))))

(defn validate-date
  "The published date of the URL. Should be in yyyy-mm-dd format."
  [string]
  (when-not (re-matches #"^\d{4}-\d{2}-\d{2}$" string)
    (condition :invalid-date (restarts {:date string})
               (error "Published date not in valid format"))))

(defn parse-csv-file [file]
  (with-open [f (reader file)]
    (mapv #(str/split % #",") (line-seq f))))

(declare validate-field)

(defn validate-csv:1:no-handlers [file]
  (let [[headers & rows :as all-rows] (parse-csv-file file)]
    (map (fn [line-number row]
           (if (not= (count row) (count headers))
             (condition :wrong-field-count (restarts {:line-number line-number})
                        (error "Number of fields doesn't equal number of headers."))
             (manage [any? (fall-through #(assoc % :line-number line-number))]
               (mapv validate-field headers row))))
         (range 2 (count all-rows))
         rows)))

(def validators   {"url" validate-url
                   "rating" validate-rating
                   "visitors" validate-visitors
                   "date" validate-date})

(defn validate-field [header value]
  (if-let [f (validators header)]
    (f value)
    (condition :invalid-header (restarts {:header header}))))


(defn validate-csv:2:ignore-errors [file]
  (let [[headers & rows :as all-rows] (parse-csv-file file)]
    (map (fn [line-number row]
           (manage [:continue-next-row (result! nil)]
             (if (not= (count row) (count headers))
               (condition :wrong-field-count (restarts {:line-number line-number})
                          (error "Number of fields doesn't equal number of headers."))
               (manage [any? (fall-through #(assoc % :line-number line-number))]
                 (manage [:continue-next-field (result! nil)]
                   (mapv validate-field headers row))))))
         (range 2 (count all-rows))
         rows)))

(defn validate-csv [file]
  (retryable [file] [:retry-file (retry! file)]
    (let [[headers & rows :as all-rows] (parse-csv-file file)]
      (doall
       (map (fn [line-number row]
              (manage [:continue-next-row (result! nil)]
                (if (not= (count row) (count headers))
                  (condition :wrong-field-count (restarts {:line-number line-number})
                             (error "Number of fields doesn't equal number of headers."))
                  (manage [any? (fall-through #(assoc % :line-number line-number))]
                    (manage [:continue-next-field (result! nil)]
                      (mapv validate-field headers row))))))
            (range 2 (count all-rows))
            rows)))))

(defn list-csv-errors [file]
  (let [result (atom [])]
    (manage [any? (restart-any :continue-next-field :continue-next-row)]
      (manage [any? (fall-through ^:restart (fn [error]
                                              (swap! result conj error)
                                              error))]
        (validate-csv file)))
    @result))


(list-csv-errors "test/tutorial.csv")
