(ns expo.eval
  (:use [clojure.core.match :only [match]]))

(defmacro symbol-macrolet*
  "Direct symbol-macrolet, expands defined forms in body
 immediately without macroexpanding rest of body."
  [mdefs & body]
  (let [sym-map (apply hash-map mdefs)]
    (assert (every? symbol? (keys sym-map)))
    (cons 'do (clojure.walk/postwalk-replace sym-map body))))

(defn transformation-fn [[name & _ :as fsrc]]
  (let [f (eval (cons 'fn fsrc))]
    (fn [x]
      (match [x]
        [(([(_ :guard #(= name %)) & args] :seq) :guard list?)] (apply f args)
        :else x))))

(defmacro macrolet*
  "Direct macrolet, expands defined forms in body
 immediately without macroexpanding rest of body.
Like macros, will recursively walk inside expanded forms, 
so be careful not to write infinite loops. Quotes will
not halt evaluation. Definitions are subject to rewriting
by previously defined forms."
  [mdefs & body]
  (if (empty? mdefs)
    body
    (let [[mdef & mdefs'] mdefs]
      (->> `(~mdefs' ~body)
           (clojure.walk/prewalk (transformation-fn mdef))
           (cons `macrolet*)))))
