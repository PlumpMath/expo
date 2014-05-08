(ns expo.eval
  (:use [clojure.core.match :only [match]])
  (:require [clojure.tools.macro]))

(defn eval-in-ns
  "Evaluates frm in namespace, retuns to
calling namespace, and returns value of
evaluation."
  ([namespace frm]
     (let [old-ns *ns*
           p (promise)]
       (in-ns namespace)
       (deliver p (eval frm))
       (in-ns (ns-name old-ns))
       @p)))

(defmacro symbol-macrolet
  "Same as clojure.tools.symbol-macrolet. Define local 
symbol macros that are used in the expansion of exprs.
The syntax is the same as for let forms."
  [symbol-bindings & exprs]
  `(clojure.tools.macro/symbol-macrolet ~symbol-bindings ~@exprs))

(defmacro symbol-macrolet*
  "Direct symbol-macrolet, expands defined forms in body
 immediately without macroexpanding rest of body."
  [mdefs & body]
  (let [sym-map (apply hash-map mdefs)]
    (assert (every? symbol? (keys sym-map)))
    (cons 'do (clojure.walk/postwalk-replace sym-map body))))

(defmacro macrolet 
  "Same as clojure.tools.macro/macrolet. Define 
local macros that are used in the expansion of exprs. The
syntax is the same as for letfn forms."
  [& args]
  `(clojure.tools.macro/macrolet ~@args))

(defn transformation-fn [[name & _ :as fsrc]]
  (let [f (eval (cons 'fn fsrc))]
    (fn [x]
      (match [x]
        [(([(_ :guard #(= name %)) & args] :seq) :guard list?)] (apply f args)
        :else x))))

(defn to-fixpoint [f x]
  (loop [a (f x)]
    (let [b (f a)]
      (if (= a b) b (recur b)))))

(defmacro macrolet*
  "Direct macrolet, expands defined forms in body
 immediately without macroexpanding rest of body.
Like macros, will recursively walk inside expanded forms, 
so be careful not to write infinite loops. Quotes will
not halt evaluation. Definitions are subject to rewriting
by previously defined forms."
  [mdefs & body]
  (if (empty? mdefs)
    (cons 'do body)
    (let [[mdef & mdefs'] mdefs]
      (cons 'macrolet*
            (to-fixpoint ;; currently will bail on fixpoint macro, leaving free variable. Fix this.
             (partial clojure.walk/prewalk
                      (transformation-fn mdef))
             `[~mdefs' ~@body])))))
