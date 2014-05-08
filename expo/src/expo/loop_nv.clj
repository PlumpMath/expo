(ns expo.loop-nv
  (:require [riddley.walk :as rw]))

(declare recur-nv)

(defn recur-nv-sym? [sym]
  (and (symbol? sym)
       (= #'recur-nv (ns-resolve *ns* sym))))

(defn expand-recur-nv [form loop-args]
  {:pre (recur-nv-sym? (first form))}
  (let [[_ & params] form
        updates (apply hash-map params)]
    (let [us (set (keys updates))
          ls (set loop-args)]
      (assert (clojure.set/subset? us ls)
              (apply str "recur-nv form references parameters not defined in loop-nv: "
                     (interpose ", " (remove ls (take-nth 2 params))))))
    (->> loop-args
         (map (merge (zipmap loop-args loop-args) updates))
         (cons 'recur))))

(defmacro loop-nv
"Identical to loop, except admits recur-nv expressions in addition
 to normal recur expressions. 
 
  recur-nv takes a series of name-value pairs, like bindings in a let
 expression. The names must be of local bindings defined in the loop-nv
 expression. The values associated with those names in the recur-nv 
 expression will be bound to those names in the next loop. Names that 
 do not occur in the recur-nv expression will have the same values
 next loop. The same name may appear in a name-value pair more than
 once, in which case the rightmost pair takes priority.

 (loop-nv [x 0, y 10, loop-count 0]
   (let [diff (Math/abs (- x y))]
     (cond (zero? diff)
           [x y loop-count]
		       
           (odd? diff)
           (recur-nv loop-count (inc loop-count)
                     x (inc x))
		       
           :else
           (recur-nv loop-count (inc loop-count) 
                     y (dec y)))))

 ;; => [5 5 10]
"
  [decls & body]
  (let [loop-args (take-nth 2 decls)
        recur-nv-form? (fn [x]
                         (and (seq? x)
                              (recur-nv-sym? (first x))))
        body' (rw/walk-exprs recur-nv-form?
                             (fn [form] (expand-recur-nv form loop-args))
                             (fn [x] (and (seq? x) (#{'loop 'fn} (first x))))
                             body)]
    `(loop ~decls ~@body')))
