(ns core-test
  (:require  [clojure.test :as t]
             [conditions :refer [manage condition lazy-conditions handler-cond restart-cond restart restarts
                                 retryable retryable-fn* retry! result! condition*
                                 remap required sibling fall-through error exception default handle trace]]))

(t/deftest various-handler-types
  (let [results
        (manage [:even (remap :thing reverse required)
                 :odd (sibling :even vector)]
          (manage [:even (fall-through range)]
            (manage [:even (fall-through dec)
                     :thing (partial map float)
                     :str (error "nein")]
              (lazy-conditions
               (map (fn [i]
                      (manage [:str #(apply str (reverse %))]
                        [(condition :str "!ereht ih" (exception Exception "Failed to str"))
                         (condition (if (odd? i) :odd :even) i required)]))
                    (range 100 0 -1))))))]
    ;; :string gets correctly reversed
    ;; :odd condition gets (map float (reverse (vector %)))
    ;; :even condition gets (map float (reverse (range (dec %))))
    ;; All of this is done lazily outside of the manage blocks using captured handlers
    (t/is (= [["hi there!" [2.0 1.0 0.0]]
              ["hi there!" [3.0]]
              ["hi there!" [0.0]]
              ["hi there!" [1.0]]]
           (->> results
                (drop 96)
                (take 4))))))


(t/deftest default-handlers
  (t/is (= "\"this happened\" 1\n"
           (with-out-str
             (t/is (nil? (condition :boo 1 (default (fn [x] (prn "this happened" x)))))))))
  (t/is (= "No 1"
           (with-out-str
             (t/is (nil? (condition :boo 1 (default (partial print "No"))))))))
  (t/is (= "\"No 1 \" :x\n\"No 2 \" :x\n"
           (with-out-str
             (manage [:boo (handle (fn [x] (prn "No 1 " x) :continue))]
               (t/is (nil? (condition :boo :x (handle (fn [x] (prn "No 2 " x))))))))))
  (t/is (= "and then :boo 1\n"
           (with-out-str
             (t/is (= 1 (condition :boo 1 (trace "and then")))))))
  (t/is (= ":boo 1\n"
           (with-out-str
             (t/is (= 1 (condition :boo 1 trace)))))))


(t/deftest cond-handlers
  (manage [:restart/x (restart-cond :c #(:data %)
                                    :d (error "Oh noes!"))
           :cond/x (handler-cond :a (default 99)
                                 :b "It's still B"
                                 :c identity
                                 :d (error "Oh bother"))]
    (manage [:restart/x (restart-cond :a (restart :do-a)
                                      :b "It's B")]
      (let [r (restarts {:a true :b true :c true :d true}
                :do-a "A+")
            arg {:a true :b true :c true :d true}]
        (t/testing "different flavors of restart-cond "
          (t/is (= "A+"
                   (condition :restart/x r (error "Darn this is wrong"))))
          (t/is (= "It's B"
                   (condition :restart/x (update r :data dissoc :a) (error "Darn this is wrong"))))
          (t/is (= {:c true :d true}
                   (condition :restart/x (update r :data dissoc :a :b) (error "Darn this is wrong"))))
          (t/is (thrown? clojure.lang.ExceptionInfo #"Oh noes!"
                         (condition :restart/x (update r :data dissoc :a :b :c) (default "Darn this is wrong"))))
          (t/is (= :golden
                   (condition :restart/x (update r :data dissoc :a :b :c :d) (default :golden)))))


        (t/testing "different flavors of handler-cond "
          (t/is (= 99
                   (condition :cond/x arg (error "Darn this is wrong"))))
          (t/is (= "It's still B"
                   (condition :cond/x (dissoc arg :a) (error "Darn this is wrong"))))
          (t/is (= {:c true :d true}
                   (condition :cond/x (dissoc arg :a :b) (error "Darn this is wrong"))))
          (t/is (thrown? clojure.lang.ExceptionInfo #"Oh bother"
                         (condition :cond/x (dissoc arg :a :b :c) (default "Darn this is wrong"))))
          (t/is (= :golden
                   (condition :cond/x (dissoc arg :a :b :c :d) (default :golden)))))))))

(t/deftest with-retryable-fn
  (def rf (retryable-fn* nil [{}] [h x]
              [:x #(retry! (inc %))]
            (if (= 41 x)
              (condition* h :x x)
              (str x))))
  (t/is (= "42" (rf 41))))


(defn do-something [x]
  (if (== 0 x)
    (str "never see this " (condition :x x))
    (/ 1 x)))

(t/deftest with-retry
  (t/is (= "-> 1/10"
           (let [x 0]
             (str "-> "
                  (retryable [x]
                      [:x #(retry! (+ 10 %))]
                    (do-something x))))))

  (t/is (= "-> 1/5"
           (let [x 5]
             (str "-> "
                  (retryable [x]
                      [:x #(retry! (+ 10 %))]
                    (do-something x)))))))


(t/deftest with-result
  (t/is (= "-> nevermind"
           (let [x 0]
             (str "-> "
                  (retryable [x]
                      [:x (fn [_] (result! "nevermind"))]
                    (do-something x))))))
  (t/is (= "-> 1/3"
           (let [x 3]
             (str "-> "
                  (retryable [x]
                      [:x (fn [_] (result! "nevermind"))]
                    (do-something x)))))))
