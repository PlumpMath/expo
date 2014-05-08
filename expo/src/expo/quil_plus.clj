(ns expo.quil-plus
  (:require quil.core
            potemkin
            [clojure.set :as sets]
            [clojure.string :as string]
            [serializable.fn :as sfn])
  (:use [clojure.pprint :only [pprint, print-table]]))

;; setup ----------------------------------------------------------

;; absorb quil.core:
(let [qcvars (->> 'quil.core
                  find-ns
                  ns-publics
                  keys)]
  (eval
   (list 'potemkin/import-vars
         (vec (cons 'quil.core qcvars)))))

;; absorb serializable.fn:
(potemkin/import-vars [serializable.fn fn])


;; printing conveniences ------------------------------------------

(defmacro prinlim [{:keys [level length] :or {level 5 length 5}}
                   & body]
  `(binding [*print-level* ~level
             *print-length* ~length]
     ~@body))

(defn qwik-pe [xs]
  (doseq [x (sort (map str xs))]
    (println x)))

(defmacro printval [& forms]
  (cons 'do
        (for [form forms]
          `(println (str ~(str form) " = " ~form)))))

;; repl interaction  ----------------------------------------------

(declare push-do)

(defn fire-promise [f] ;blocks. for repl
  (let [p (promise)]
     (push-do #(deliver p (f)))
     @p))

;; mouse conveniences ---------------------------------------------

(defmacro let-mouse [[x y] & body]
  `(let [~x (mouse-x)
         ~y (mouse-y)]
     ~@body))

;; exception state ------------------------------------------------

(def exception-storage (atom [] :validator (fn [x] (every? #(instance? java.lang.Exception %) x))))

(defn flush-exception-storage [] (reset! exception-storage []))

;; vocabulary for interacting with Processing settings ------------

(def shape-modes #{:corner :corners
                   :radius :center})
(def stroke-caps #{:square :project :round})
(def negs        #{:ns :nf})
(def mono-syms   (sets/union shape-modes
                             stroke-caps
                             negs))

(defn qwik-set [& params]
  (do (doseq [[k v] (partition 2 (remove mono-syms params))]
        (case k
          :s    (if (coll? v)
                  (apply stroke v)
                  (stroke v))
          :sw   (if (coll? v)
                  (apply stroke-weight v)
                  (stroke-weight v))
          :f    (if (coll? v)
                  (apply fill v)
                  (fill v))
          :font (if (coll? v)
                  (apply text-font v)
                  (text-font v))
          :fs   (text-size v)))
      (doseq [x (filter mono-syms params)]
        (cond
         (shape-modes x) (do (ellipse-mode x)
                             (rect-mode x))
         (stroke-caps x) (stroke-cap x)
         (= :ns x)       (no-stroke)
         (= :nf x)       (no-fill)))))

(defmacro qwik-set*
  "macro version of qwik-set. Faster."
  [& params]
  (let [unpack       (fn [f v] (cond
                               (vector? v) `(~f ~@v)
                               (list? v) ~v
                               :else     `(~v)))
        exprs-1 (for [[k v] (partition 2 (remove mono-syms params))]
                  (case k
                    :s `(stroke ~(unpack 'color v))
                    :sw `(stroke-weight ~v)
                    :f `(fill ~(unpack 'color v))
                    :font (if (vector? v)
                            `(text-font ~@v)
                            `(text-font ~v))
                    :fs `(text-size ~v)))
        exprs-2 (for [x (filter mono-syms params)]
                  (cond
                   (shape-modes x) `(do (ellipse-mode ~x)
                                        (rect-mode ~x))
                   (stroke-caps x) `(stroke-cap ~x)
                   (= :ns x) `(no-stroke)
                   (= :nf x) `(no-fill)))]
    (cons 'do (concat exprs-1 exprs-2))))

(defmacro matrix-block [& body]
  `(do (push-matrix)
       (let [return# (do ~@body)]
         (pop-matrix)
         return#)))

(defmacro px-block [pxs-name & body]
  `(let [~pxs-name (pixels)
         ret# (do ~@body)]
     (update-pixels)
     ret#))

(defmacro shape-block [& body]
  `(do (begin-shape)
       (let [return# (do ~@body)]
         (end-shape)
         return#)))

;; cumulative live coding -----------------------------------------

;; top-level let-over-lambda's kind of obnoxious,
;; but we really don't want to be worrying about
;; messing up this piece of state while live coding


(let [do-vec (atom [])]

  (defn push-do [f]
    (swap! do-vec #(conj % f)))

  (let [history-atm (atom [[#(background 0)]]
                          :validator #(and (vector? %)
                                           (every? vector? %)))
        present-atm (atom 0)
        paused-atm (atom false)]

    (defn history [] @history-atm)
    
    (defn present [] @present-atm)

    (defn paused? [] @paused-atm)
    
    (defn pause [] (swap! paused-atm not))

    (defn set-present [n]
      (reset!
       present-atm
       (min (dec (count @history-atm))
            (max 0
                 n))))

    (defn undo []
      (set-present (dec @present-atm)))

    (defn redo []
      (set-present (inc @present-atm)))

    (defn procedure [] (@history-atm @present-atm))

    (defn put-procedure [procedure]
      (swap! present-atm inc)
      (reset! history-atm
              (conj
               (vec (first (split-at @present-atm
                                     @history-atm)))
               procedure)))

    (defn swap-fn
      ([] (let [pres @present-atm]
            (swap-fn pres (dec pres))))
      ([n] (swap-fn @present-atm n))
      ([n m] (let [p0 (procedure)]
               (put-procedure
                (-> (procedure)
                    (assoc m (p0 n))
                    (assoc n (p0 m)))))))
    
    (defn change-procedure [f]
      (put-procedure (f (procedure))))

    (defmacro var-catcher [x]
      (or (when (or (not (symbol? x))
                    (find &env x))
            x)
          (when-let [v (find-var (symbol (name (ns-name *ns*)) (name x)))]
            `(quote ~v))
          x))
    
    (defmacro push-fn [f]
      `(put-procedure
        (conj (procedure)
              (var-catcher ~f))))
    
    (defn clear-history []
      (reset! history-atm [[]])
      (reset! present-atm 0))
    
    (defn print-history []
      (let [hist @history-atm
            curl @present-atm]
        (doseq [i (range (count hist))]
          (let [x (hist i)]
            (println (format
                      "%1$s %2$3d | %3$s %1$s"
                      (if (= i curl)
                        "*"
                        " "),
                      i,
                      (apply str
                             (interpose "; "
                                        (map
                                         #(:timestamp (meta %))
                                         (hist i))))))))))

    (defn print-program [& {:keys [index] :or {index (dec (count @history-atm))}}]
      (if (and (< -1 index)
               (< index (count @history-atm)))
        (let [frmt #(format "%-10.5s %s" (str %) %2)]
          (doseq [[i x] (map list (range) (procedure))]
            (try (let [lines (->> (if (var? x) (var-get x) x)
                                  pr-str
                                  read-string
                                  pprint
                                  with-out-str
                                  string/split-lines)]
                   (println (frmt i (first lines)))
                   (doseq [line (rest lines)]
                     (println (frmt nil line))))
                 (catch java.lang.RuntimeException e
                   (println (frmt i "\"Unreadable form (probably unserializable fn)\""))))))
        (println
         (str index " is a bad index:\n"
              (with-out-str (printval (count @history-atm)))
              "\n"))))
    
    (defn resume []
      (do (undo)
          (reset! paused-atm false)))
    
    (defn run []
      (let [dq @do-vec]
        (try
          (when (not (empty? dq))
            (doseq [f dq] (f)))
          (catch Exception e
            (pause)
            (background 255 0 0)
            (swap! exception-storage (fn [es] (conj es e))))
          (finally (reset! do-vec [])))
        (try
          (when (not @paused-atm)
            (doseq [f (procedure)] (f)))
          (catch Exception e
            (pause)
            (background 255 255 0)
            (swap! exception-storage (fn [es] (conj es e)))))))))

(defn save-program [f & opts]
  (let [p (->> (procedure)
               (map #(with-out-str (print %)))
               (clojure.string/join "\n"))]
    (apply spit f p opts)))
