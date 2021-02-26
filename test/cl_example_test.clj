(ns cl-example-test
  (:require [clojure.test :as t]
            [conditions :refer [restart-with manage condition condition* restart restarts lazy-conditions error]]))

;; from https://wiki.c2.com/?CommonLispConditionSystem

(declare determine-infinity reciprocal-of)

(defn high-level-code []
  [(manage [:on-zero-denominator
            (restart-with
             (fn [condition arg default]
               (println (str condition " " arg " " (:message (meta default)) ": Just return zero"))
               'return-zero))]
     (determine-infinity))

   (manage [:on-zero-denominator (restart 'return-value 1)]
     (determine-infinity))

   (manage [:on-zero-denominator (restart 'recalc-using 2)]
     (determine-infinity))

   (manage [:on-zero-denominator (restart 'just-continue)]
     (determine-infinity))])

(defn determine-infinity []
  (manage ['just-continue nil]
    (reciprocal-of 0)))

(defn reciprocal-of [value]
  (if (not= value 0)
    (/ 1 value)
    (condition :on-zero-denominator
               (restarts value
                         'return-zero 0
                         'return-value identity
                         'recalc-using reciprocal-of)
               (error "cannot divide by zero"))))


(t/deftest c2-example
  (t/is (= ":on-zero-denominator 0 cannot divide by zero: Just return zero\n"
           (with-out-str
             (t/is (= [0 1 1/2 nil]
                      (high-level-code)))))))


;; This test comes from an explainer on the CL condition system:
;; http://www.nhplace.com/kent/Papers/Exceptional-Situations-1990.html

(def normal-color {:apple :green :kiwi :brown :sushi :pink})

(defn correct-color? [food]
  (= (normal-color (:type food)) (:color food)))

(defn robot-butler [food-items]
  {:enjoy
   (lazy-conditions
    (keep (fn [food]
            (if (correct-color? food)
              food
              (condition :bad-food-color
                         (restarts {:food (:type food)
                                    :color (:color food)
                                    :expected-color (normal-color (:type food))}
                                   'add-food-coloring (fn [color] (color food))
                                   'lgtm food
                                   'toss nil)
                         (error "malfunction"))))
          food-items))})

(t/deftest exceptional-situations
  (t/is (= {:enjoy [{:type :apple, :color :green}
                    {:type :sushi, :color :grey}
                    {:type :sushi, :color :blue}
                    {:type :kiwi, :color :grey}]}
           (let [rotate #(conj (subvec % 1) (first %))
                 solution (atom ['lgtm 'toss ['add-food-coloring #(assoc % :color :grey)]])]
             (manage [:bad-food-color
                      (restart-with
                       (fn [c arg d]
                         (first (swap! solution rotate))))]
                     (robot-butler [{:type :apple :color :green}
                                    {:type :apple :color :blue}
                                    {:type :sushi :color :blue}
                                    {:type :sushi :color :blue}
                                    {:type :kiwi :color :blue}
                                    {:type :kiwi :color :blue}]))))))

(defn available-restarts [restart]
  (set (keys (apply merge (:handlers restart)))))

(t/deftest learn-restarts-from-exception
  (t/is (thrown-with-msg? clojure.lang.ExceptionInfo #"malfunction"
                          (doall (:enjoy (robot-butler [{:type :apple :color :red}])))))
  (t/is (= #{'lgtm 'toss 'add-food-coloring}
           (try (doall (:enjoy (robot-butler [{:type :apple :color :red}])))
                (catch Exception e
                  (available-restarts (:value (ex-data e))))))))
