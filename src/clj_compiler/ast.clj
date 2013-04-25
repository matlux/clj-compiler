(ns clj-compiler.ast
  (:use [clojure.core.match :only (match)]
        clojure.walk))

;; General thoughts around Clojure compilation
;; Compilation could happen on systems other than the JVM,
;; consider LLVM -
;; Clojure already has model for MT via Refs, Atoms and STM
;; escape analysis could be usefully employed on closures
;; instantaneous startup

(defn invoke? [f] (every-pred seq? not-empty #(= (first %) f)))
(def quoted? (invoke? 'quote))
(def let*?   (invoke? 'let*))

(defn prewalk-form [f form]
  (if (quoted? form)
    form
    (walk (partial prewalk-form f) identity (f form))))

(defn map-vals [f m]
  (apply assoc m (interleave (keys m) (map f (vals m)))))

(defn common-types
  "Return a set of types which are common to both a and b"
  [a b] )

(defn conj? [coll x] (if (nil? x) coll (conj coll x)))

(defn my-walk [inner outer form]
  (cond
   (list? form) (outer (apply list (map inner form)))
   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (doall (map inner form)))
   (map? form) (outer (into form (map inner form)))
   (coll? form) (outer (into (empty form) (map inner form)))
   :else (outer form)))

(defn prewalk-expr [f ast]
  (into ast (map (fn [[k v]] [k (f v)]) ast)))

(defmacro enrich-map [m & enrich]
  (if-let [[k v] (not-empty (take 2 enrich))]
    `(enrich-map
      (let [m# ~m {:keys ~(first v)} m#] (assoc m# ~k ~@(rest v)))
      ~@(drop 2 enrich))
    m))

(declare ast)
(declare parse-form)

(defn fn-ast [form])

(def ^dynamic *resolver* resolve)

(defn parse-do [locals & exprs]
  (cond
   (empty? exprs) nil
   (empty? (rest exprs)) (parse-form locals (first exprs))
   :else
   (enrich-map
    {:type :do-expr
     :exprs (map (partial parse-form locals) exprs)}
    :result-type ([exprs] (:result-type (last exprs))))))

(defn parse-if [locals & params]
  (let [[test then else] (map (partial parse-form locals params))]
    {:type :if-expr :test test :then then :else else
     :result-type (common-types (:result-type then) (:result-type else))}))

;; need to add bindings to locals

(defn make-map [f coll] (zipmap coll (map f coll)))

(defn parse-single-fn [locals args & exprs]
  (let [arg-type (fn [sym] (or (:tag (meta sym)) Object))]
    (enrich-map
     {:type :single-fn-expr
      :args (map vector args (map arg-type args))
      :var-args? (boolean (some #{'&} args))}
     :body ([args] (apply parse-do (into locals args) exprs))
     :used ([body args] (apply dissoc (:used body) args))
     :result-type ([body] (:result-type body))
     )))

;; if type hints are present, cannot be overriden, becomes a constraint
;; given an AST transform into a type function
;; replace out closures
;; var-args arity held separately

(defn type-function [arities closures] (fn [& types]))

;; the result type of a function is a function of its input parameters
;; we can overload function(s) with different parameter types?

(defn find-arity [n arities]
  (letfn [(handle? [{:keys [args var-args?]}]
            (or (= (count args) n) (and var-args? (>= n (dec (count args))))))]
    (->> arities (filter handle?) first)))

(defn parse-fn* [locals & params]
  (letfn [(parse2 [locals & arities]
            (enrich-map
             {:type :fn-expr
              :arities (map (partial apply parse-single-fn locals) arities)}
             :used    ([arities] (apply merge (map :used arities)))
             :result-type ([arities]
                             (fn [& arg-types]
                               (-> (find-arity (count arg-types) arities) :result-type)))))
          (parse1 [locals & arrities]
            (apply parse2 locals )
              (if (vector? (first arrities))
                (list arrities)
                arrities))]
    (if (symbol? (first params))
      (-> (apply parse1 (assoc locals (first params) :this) (rest params))
          (update-in [:used] disj (first params))
          (assoc :name (first params)))
      (apply parse1 locals params))))

(defn parse-let* [locals bindings & exprs]
  (if (empty? bindings)
    (apply parse-do locals exprs)
    (let [[sym val & rst] bindings]
      (enrich-map
       {:type :let-one-expr
        :symbol sym
        :value (parse-form locals val)}
       :expr ([value] (let [new-locals (assoc locals sym (:result-type value))]
                        (apply parse-let* new-locals rst exprs)))
       :result-type ([expr] (:result-type expr))))))

(defn parse-symbol [locals symbol]
  (if (contains? locals symbol)
    {:type :local-symbol-expr
     :symbol symbol
     :result-type (locals symbol)}
    {:type :global-symbol-expr}))

(def parsers
  {'do parse-do
   'if parse-if
   'fn* parse-fn*
   'let* parse-let*
   '. parse-dot
   'quote #(QuoteExpr. %&)
   ;; 'loop or 'loop*;; TODO
   })

;; execute with a value and a type, or undefined and a type
;; how does if work?
;; test returns a truthy value or 'undefined'
;; determines which branch to execute or both of them

;; is Nil a type?

;; checks on types can be properly handled


;; transform a fn-form into a type-function
;; if blocks can be re-written
;;
;; only need to cast at last possible point in time

;; use polymorphic dispatch
;; given a map of locals to
(defn fn-result-type
  "Given the function form with argument constraints fundamental result-type"
  [fnform & arg-types]
  Object)

;; symbols are resolved to global or local
;; what about the case where a var (static or dynamic) or a java method is invoked?
;; need to disabiguate those cases
(defn parse-invoke-expr [locals f & args]
  (enrich-map
   {:type :invoke-expr
    :f (parse-form locals f)
    :args (parse-form locals args)}
   :result-type
   ([f args]
      (if-let [rt (:result-type f)]
        (cond
         (fn? rt) (apply rt (map :result-type args))
         :else Object)
        Object
        ))))

(defn parse-seq [locals f & params]
  (apply (or (parsers f) parse-invoke-expr) locals params))

(defn hash-map? [m] (instance? clojure.lang.PersistentHashMap m))
(defn array-map? [m] (instance? clojure.lang.PersistentArrayMap m))

;; transform vectors, maps etc into function calls?
;; embed literals?
;; pass maps through, or modify function

(defn ast [resolver form]
  (letfn [(parse-form [locals form]
            )
         ]

    )
  (let [m (macroexpand form)]
    (cond
     (and (seq? m) (not-empty m)) (apply parse-seq m)
     (vector? m) (invoke-expr clojure.core/vector (map parse-form m))
     (map? m)    (invoke-expr clojure.core/array-map (map parse-form (apply concat (seq m))))
     (set? m)    (invoke-expr clojure.core/hash-set (map parse-form m))
     (coll? m) (clojure.walk/walk parse-form identity m)
     :else m)))

#_(defn symbol-ast [symbol lexical-symbols]
  (cond
   (symbol-ns m) m ;; TODO
   (lexical-symbols m) (LocalSymbolExpr. m)

   ;; perform the lookup for the global symbol?
   :else               (GlobalSymbolExpr. m)))


;; thoughts on namespaces
;; update to namespace should be atomic
;; co-ordinated through refs?

;; namespace accessed through (dosync) mechanism
;; lookups performed in similar manner, get latest value of map
;; can update namespace in neat transactional manner
;; whats the effect? when performing symbol resolution pass in 'state of world'


;; step 1 build simple AST
;; step 2 resolve symbols
;; step 3 add types

;; a lexical symbol resolves to an expression - with a type
;; knowing the type at point of usage is important
;; as we progress keep a map if symbol->type
;; direct links can be typed, vars are only objects

(defn resolve-symbols
  "Run through the AST resolving local symbols to Global, Var, Local or Unresolvable"
  [ast lookup]

  )

;; each expression has a result-type
;; can follow in strict order
;; becomes mechanical
;; as a symbol is typed, type its usages in the children ?
;; always returns a typed ast
;; each expression associates meta if avaliable

;; simplifies interface

; typing is two things, returning result type and annotating
;; LetOneExpr ... gets


(defn type-ast
  "scoped is a map of lexical scoped symbols to types - is it neccessary?"
  [ast]

  )

(defn index-ast [ast])


#_(defn ast
  "Transform a form into a simple AST representation consisting of the following types:
   DoExpr, IfExpr, FnExpr, LetOneExpr, SymbolExpr, InvokeExpr"
  [form]
  (let [m (macroexpand form)]
    (cond
     (seq? m) (seq-ast m)
     (coll? m) (into (empty m) (map ast m))
     (symbol? m) (SymbolExpr. m)

     :else (literal-expr m))))

;; mark purity on each function
;; determine which symbols aren't passed to impure functions

;; which expressions 'leak' out of a function

;; three points of leakage
;; 1 - return value
;; 2 - impure functions, what if an impure function is passed as an argument
;; 3 - exceptions

;; track pure functions

;; is map pure?
;; functions may be pure depending on their arguments
;; in that case map is pure?

;; what are the parameter types for a lambda?
;; is a function pure?
;; if a function is passed no functions, answer can be

;; providing types might be a multi-pass affair

(defn type-ast
  "ast - a partially typed AST, returns a fully typed AST"
  [ast]

  )

;; break out lambdas? from AST
;; return a form where depth first lambdas are returned first

(defn flatten-lambdas
  "Return depth-first sequence of fn expressions representing original lambdas"
  [ast]
  ;; walk the tree depth first,
  ;; function returns two things, list of lambdas and resultant expression

  )

(defn lambdas
  "Returns a sequence of lambda expressions with lexical closures"
  [ast])

(defn replace-lambdas
  "Replaces lambdas with function constructor calls"
  [ast]
  )

;; for each lambda derive a set of closures
;; each lambda is firstly given a classname
;; a constructor can be derived given the classname
;; a function to add closures

;; if passed a function which is higher-order, can only
;; assume passed function is 'impure', function is 'unbound'
;; any inputs to unbound function are determined to leak

;; the greater the "scope" of the function the more optimized it can
;; become

;; consider (map inc (iterate inc 0))


;; map through all types

;; define a function for construction? - multiple indirection, maybe slower

;; if's pass through
;; only fn* and let* have multi arr

(defn simplify-fn* [& params]
  ;; do we have a name?
  )

;; examples

(comment
(pprint (parse-form '(fn foo [x] (if (< x 1) x (+ x 2)) (+ 2 3))))
  )
