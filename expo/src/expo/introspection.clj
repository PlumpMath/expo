(ns tsg.kits.introspection
  (:require
   [clojure.pprint :as pprint]
   [clojure.reflect :as reflect]))

(defn apropos+
  "Given a regular expression or stringable thing, return a seq of
all definitions in all currently-loaded namespaces that match the
str-or-pattern."
  [str-or-pattern]
  (let [matches? (if (instance? java.util.regex.Pattern str-or-pattern)
                   #(re-find str-or-pattern (str %))
                   #(.contains (str %) (str str-or-pattern)))]
    (sort-by (comp ns-name :ns)
      (keep
        (fn [ns]
          (when-let [hits (->> ns ns-publics keys (filter matches?) seq)]
            {:ns ns
             :hits (sort hits)}))
        (all-ns)))))

(defn aproprint [str-or-pattern]
  (clojure.pprint/pprint (apropos+ str-or-pattern)))

(defn print-publics [ns]
  (let [nim (ns-interns ns)]
    (->> (keys nim)
         (group-by nim)
         vals
         sort
         (map #(if (= 1 (count %)) (first %) %))
         pprint)))

(defn print-aliases []
  (pprint (sort (ns-aliases *ns*))))

(defn reflect-methods [x]
  (->> x
       clojure.reflect/reflect
       :members
       (filter :return-type)
       (sort-by :name)))

(defn reflect-fields [x]
  (->> x
       clojure.reflect/reflect
       :members
       (remove :parameter-types)
       (sort-by :name)))
