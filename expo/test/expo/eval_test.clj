(ns expo.eval-test
  (:require [clojure.test :refer :all]
            [expo.eval :refer :all]))

(deftest macrolet*-test
  (is (= [:b :a]
         (macrolet* [(m1 [x y] [y x])]
           (m1 :a :b))))
  (is (= [:b :a]
         (macrolet* [(m1 [[x y]] [y x])]
           (m1 [:a :b]))))
  (is (= [:b :a]
         (macrolet* [(m1 [[x y]] [y x])
                     (m2 [x] `(reverse ~x))]
           (m2 (m2 (m1 [:a :b])))))))

(deftest macrolets-composition-test
  (is (= [:a :b]
         (macrolet* [(m1 [[x y]] [y x])]
           (macrolet [(m2 [[x y]] [y x])]
             (m2 (m1 [:a :b])))))))

