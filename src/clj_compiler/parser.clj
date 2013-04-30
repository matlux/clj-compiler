(ns clj-compiler.parser
  (:use clojure.set))


(declare parse-form)

;; step 1 normalize form
;; normalized from is simpler, + also fully executable

;; step 2 read normalized form into records/maps

;; for a function have a special symbol? such that
;; compilation proceeds ok with a normalized form?
;; symbol might be used in form definition but is it used in form?

(defn map-vals [f m] (into m (map vector (keys m) (map f (vals m)))))

(defn assoc-walk
  "f returns a vector of [new-obj keys-to-map]"
  [f form]
  (let [[o ks] (f form)]
    (if (empty? ks) o (apply assoc o (interleave ks (map (partial get o) ks))))))

(defn map-walk [f form] (assoc-walk #(let [r (f %)] [r (if (map? r) (keys r) nil)]) form))

(comment
  (defrecord DoExpr [exprs] Expr)

  (defrecord IfExpr [test then else] Expr)

  (defrecord LetOneExpr [symbol value expr] Expr)

  (defrecord InvokeExpr [f args] Expr)

  (defrecord SingleFnExpr [args var-args? expr] Expr)

  (defrecord FnExpr [arities] Expr)

  (defrecord LocalSymbolExpr [symbol] Expr)

  (defrecord GlobalSymbolExpr [symbol] Expr)

  (defrecord ClosureSymbolExpr [symbol] Expr))

(defn parse-do [& exprs]
  (cond
   (empty? exprs) nil
   (empty? (rest exprs)) (parse-form (first exprs))
   :else {:expr-type :do :exprs (map parse-form exprs)}))

(defn parse-if [& params]
  (merge {:expr-type :if} (zipmap [:test :then :else] (map parse-form params))))

(defn parse-single-fn [args & exprs]
  {:expr-type :single-fn
   :args args :var-args? (boolean (some #{'&} args)) :expr (apply parse-do exprs)})

(defn parse-fn* [& params]
  (letfn [(as-list [x] (if (not (seq? (first x))) (list x) x))
          (parse1 [& arities]
            {:expr-type :fn :arities (map (partial apply parse-single-fn) (as-list arities))})]
    (if (symbol? (first params))
      (-> (apply parse1 (rest params)) (assoc :name (first params)))
      (apply parse1 params))))

(defn parse-let* [bindings & exprs]
  (if (empty? bindings)
    (apply parse-do exprs)
    (let [[sym val & rst] bindings]
      {:expr-type :let-one-expr
       :symbol sym :value (parse-form val) :expr (apply parse-let* rst exprs)})))

(defn parse-invoke-expr [f & args]
  {:expr-type :invoke-expr :f (parse-form f) :args (map parse-form args)})

;; TODO
(defn parse-dot [& exprs])

(defn parse-quote [& exprs] {:expr-type :quote-expr :expr (first exprs)})

(def parsers
  {'do parse-do
   'if parse-if
   'fn* parse-fn*
   'let* parse-let*
   '. parse-dot
   'quote parse-quote
   ;; 'loop or 'loop*;; TODO
   })

(defn parse-seq [f & params]
  (if (contains? parsers f)
    (apply (parsers f) params)
    (apply parse-invoke-expr f params)))

(defn parse-form [form]
  (let [m (macroexpand form)
        invoke-expr (fn [f args] (apply parse-invoke-expr f args))]
    (cond
     (and (seq? m) (not-empty m)) (apply parse-seq m)
     (vector? m) (invoke-expr clojure.core/vector (map parse-form m))
     (map? m)    (invoke-expr clojure.core/array-map (map parse-form (apply concat (seq m))))
     (set? m)    (invoke-expr clojure.core/hash-set (map parse-form m))
     (coll? m)   (clojure.walk/walk parse-form identity m)
     :else m)))

(defn expr? [form] (and (map? form) (contains? form :expr-type)))

(defn map-exprs [f ast]
  (if-let [to-map (not-empty (filter (comp expr? second) ast))]
    (apply assoc ast (mapcat #(update-in % [1] f) to-map))
    ast))

(defn mapunion [f coll] (apply union (map f coll)))

(defn used-symbols [ast pool]
  (cond
   (empty? pool) pool

   (= (:expr-type ast) :let-one-expr)
     (let [{:keys [symbol value expr]} ast]
       (union
        (used-symbols value pool)
        (used-symbols expr (disj pool symbol))))

   (= (:expr-type ast) :single-fn)
     (let [{:keys [args expr]} ast]
       (used-symbols expr (apply disj pool args)))

   (= (:expr-type ast) :fn)
     (mapunion #(used-symbols % (disj pool ast)) (:arities ast))

   (contains? pool ast) #{ast}

   (expr? ast) (mapunion #(used-symbols % pool) (vals ast))

   (coll? ast) (mapunion #(used-symbols % pool) ast)

   :else #{}))

(comment
  (defn if-type [{:keys [test then else]} typed-locals]
    (let [[v t] (ast-type test typed-locals)]
      (cond
       (= v ::undefined) [::undefined Object] ;; run a union over both cases
       v (ast-type then typed-locals)
       :else (ast-type else typed-locals))))

  (defn let-one-type [{:keys [symbol value expr]} typed-locals]
    (ast-type expr (assoc typed-locals symbol (ast-type value typed-locals))))

  ;; return the type as Fn and the value as a function
  (defn fn-type [{:keys [name arities]}]
    [(fn [& args]
       ;; select the correct arity from the cardinality of the args
       ) Fn]
    )

  (defn ast-type [ast typed-locals]
    (cond
     (instance? IfExpr ast) (if-type ast typed-locals)
     (instance? LetOneExpr ast) (let-one-type ast typed-locals)
     (instance? FnExpr ast)
     )
    ))

;; in this pass attach closure sets to functions
(defn resolve-symbols [ast locals]
  (cond
   (= (:expr-type ast) :let-one-expr)
     (-> ast
       (update-in [:value] resolve-symbols locals)
       (update-in [:expr] resolve-symbols (conj locals (:symbol ast))))

   (= (:expr-type ast) :single-fn)
     (update-in ast [:expr] resolve-symbols (union locals (:args ast)))

   (= (:expr-type ast) :fn)
     (-> ast
         (assoc :closures (used-symbols ast locals))
         (update-in [:arities] (fn [arity] (resolve-symbols arity (conj locals (:name ast))))))

 ;;  (symbol? ast) (if (contains? locals ast) (->LocalSymbolExpr ast) (->GlobalSymbolExpr ast))

   (expr? ast) (map-exprs #(resolve-symbols % locals) ast)

   :else ast))

;; we have a form and all local symbols have been resolved
;; next capture closures
