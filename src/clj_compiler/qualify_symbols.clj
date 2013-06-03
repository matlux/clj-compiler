(ns clj-compiler.qualify-symbols)

(def evens (partial take-nth 2))
(def odds (comp evens rest))

(defn uninterleave [coll] [(evens coll) (odds coll)])

(declare qualify-symbols)

(defn let-qualify-symbols [locals bindings & exprs]
  (let [[symbols inits] (uninterleave bindings)]
     (apply list 'let*
       (vec (interleave symbols (map qualify-symbols (reductions conj locals symbols) inits)))
       (map (partial qualify-symbols (into locals symbols)) exprs))
     ))

(defn arity-qualify-symbol [locals params & exprs]
  (cons params (map (partial qualify-symbols (into locals (remove #{'&} params))) exprs)))

(defn arities-qualify-symbols [locals & arities]
  (map (partial apply arity-qualify-symbol locals) arities))

(defn fn-qualify-symbols [locals & form]
  (if (symbol? (first form))
    (apply list 'fn* (second form)
           (apply arities-qualify-symbols (conj locals (second form)) (drop 2 form)))
    (apply list 'fn* (apply arities-qualify-symbols locals form))))

(defn qualify-symbol [sym]
  (if-let [{:keys [ns name]} (meta (resolve sym))]
    (symbol (str (ns-name ns)) (str name))
    sym))

(defn qualify-symbols
  "Recursively run through the form fully qualify any global symbols to their namespaces"
  ([form] (qualify-symbols #{} form))
  ([locals form]
     (if (symbol? form)
       (if (contains? locals form) form (qualify-symbol form))
       (letfn [(default [] (clojure.walk/walk (partial qualify-symbols locals) identity form))]
         (if (seq? form)
           (condp = (first form)
             'let* (apply let-qualify-symbols locals (rest form))
             'fn*  (apply fn-qualify-symbols locals (rest form))
             (default))
           (default))))))
