(ns tsg.kits.macros)

(defmacro symbol-macrolet*
  "Direct symbol-macrolet, expands defined forms in body
 immediately without macroexpanding rest of body."
  [mdefs & body]
  (let [sym-map (apply hash-map mdefs)]
    (assert (every? symbol? (keys sym-map)))
    (cons 'do (clojure.walk/postwalk-replace sym-map body))))

(defmacro macrolet*
  "Direct macrolet, expands defined forms in body
 immediately without macroexpanding rest of body."
  [mdefs & body]
  (let [transforms
         (reduce
           (fn [bldg [name & body]]
             (assoc bldg name
              (eval (list* 'fn name body))))
           {}
           mdefs)]
    (cons 'do
      (clojure.walk/prewalk
        (fn [x]
          (if (list? x)
            (if-let [[head & tail] (seq x)]
              (if-let [f (transforms head)]
                (apply f tail)
                x)
              x)
            x))
        body))))
