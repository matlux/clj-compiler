(ns clj-compiler.ast
  (:use [clojure.core.match :only (match)]
        clojure.walk))

(defn invoke? [f] (every-pred seq? not-empty #(= (first %) f)))
(def quoted? (invoke? 'quote))
(def let*?   (invoke? 'let*))

(defn prewalk-form [f form]
  (if (quoted? form)
    form
    (walk (partial prewalk-form f) identity (f form))))

;; have a method called id-local-symbols, returns form + count
;; method to remove-shadowing
;; given current content, increment by adding let depth?
;; output must be syntactically valid
;; each fn expr tags the closures?
;; id each local symbol, no issue with shadowing
;; each function needs to calculate its set of closures before flatten
;; types not required at the moment, just becomes a list of symbols
;; pass in a map of symbols->types returns set of symbols used in Fn
;; use a list, argument order becomes important

;; cost of thread local lookup, which is a stack
;; whats the result-type of a lambda?

(defn to-bytecode
  "Return a map of sym -> lambda forms to compile"
  [ast]
  )

(defn fn-exprs [form]

  )

(defn map-vals [f m]
  (apply assoc m (interleave (keys m) (map f (vals m)))))

;;

(defprotocol Expr
  (result-type [this])
  (add-types [this locals global-lookup]
    "locals - a map of symbol -> type
     globals - "
    )
  (resolve-symbols [this lookup]
    "symbols are resolved to local, closure or global,
     global symbols are resolved to vars, dynamic vars can only
     yield object as their type"
    )
  ;;(unbound [this local-symbols])
  ;; (flatten-lambdas [this])
  ;; (add-types [this] (assoc this :result-type (result-type this)))
  )

(defn common-types
  "Return a set of types which are common to both a and b"
  [a b] )

(defrecord IfExpr [test then else]
  Expr
  (result-type [this]
    (common-types (result-type (:then this)) (result-type (:else this))))
  (resolve-symbols [this lookup]
    )

  #_(flatten-lambdas [this]
    [(concat )
     (IfExpr. nil nil nil)]
    )
  #_(unbound [{:keys [test then else]}]
    (merge (unbound test) (unbound then) (unbound else))))

(defrecord QuoteExpr [exprs])

(defrecord DoExpr [exprs]
  Expr
  (result-type [this] (result-type (last exprs)))
  #_(unbound []))

(defn do-expr [exprs] (assoc (DoExpr. exprs) :expr-type :DoExpr))

(defrecord LetOneExpr [sym val body]
  Expr
  (result-type [this] (result-type body))
  #_(unbound [{:keys [sym val body]} local-symbols]
    (merge (unbound val local-symbols)
           (unbound body (conj local-symbols sym))))
  )

;; call the constructor with the closures
(defrecord NewLambdaExpr [class-name closures])

(defrecord loop-expr [])

(defrecord recur-expr [])

(defrecord SymbolExpr [symbol source result-type]
  Expr
  #_(unbound [{:keys [symbol]} local-symbols]
      (if (contains? local-symbols) #{} #{symbol})))

(defn symbol-expr
  "source-type is one of: :namespace :param :let :closure"
  [symbol source result-type]
  (SymbolExpr. source result-type))

(defrecord ParamExpr [symbol])

;; a symbol has different sources:
;; 1. Namespace
;; 2. Param
;; 3. Let
;; 4. Closure

(defn param-expr [symbol]
  (assoc (ParamExpr. symbol)
    :meta (meta symbol)
    :type (get (meta symbol) :tag Object)))

(defrecord GlobalSymbolExpr [symbol namespace]
  )

(defrecord LiteralExpr [literal]
  Expr
  (result-type [this] (type literal)))

(defrecord FnBody [params body]
  Expr
  (result-type [{:keys [params body]}]
    (result-type body))
  #_(unbound [{:keys [params body]} local-symbols]
    (unbound body (into local-symbols params)))
  )

(defn expr? [x] (instance? Expr x))

;; apply a transform

;; two

;; for fn name is irrelevant at compilation level
;; name transposed with recursive function call, if arrity
;; can be determined

;; base offsets relative to current function

;; step 1, assign types to all expressions
;; step 2, name all lambdas
;; step 3, split out lambas
;; step 4, compile each function in order

;; we have class level vars, these have indices and types
;; expressions can be re-written to use these class level vars

;; when attaching metadata create new function, of same type
;; copy across closures, add metadata
;; avoid deep nesting

;; is Object.clone good enough?

(defn conj? [coll x] (if (nil? x) coll (conj coll x)))

(defrecord FnExpr [name? arrities]
  Expr
  #_(unbound [{:keys [name? arrities var-args]} local-symbols]
    (apply
     clojure.set/union
     (map #(unbound % (conj? local-symbols name?)) (conj? arrities var-args)))))

(defn fn-expr [name arrities]
  (assoc (FnExpr. name arrities) :expr-type :FnExpr))

(defrecord InvokeExpr [f args])

(defn invoke-expr [f args]
  (assoc (InvokeExpr. f args) :expr-type :InvokeExpr))

(defn let-one-expr
  "When compiling to bytecode the symbol becomes irrelevant, the local index is more
   important, the type of let-one-expr is the type of the body"
  [sym val body]
  (assoc (LetOneExpr. sym val body)
    :expr-type :let-one-expr))

(defn literal-expr [literal]
  (assoc (LiteralExpr. literal)
    :expr-type :literal-expr
    :class (type literal)))

(defn if-expr [test then else]
  (assoc (IfExpr. test then else) :expr-type :IfExpr))

(defn my-walk [inner outer form]
  (cond
   (list? form) (outer (apply list (map inner form)))
   (instance? clojure.lang.IMapEntry form) (outer (vec (map inner form)))
   (seq? form) (outer (doall (map inner form)))
   (map? form) (outer (into form (map inner form)))
   (coll? form) (outer (into (empty form) (map inner form)))
   :else (outer form)))

;; step 1, fully macro-expand form
;; step 2, add do blocks where required
;; processing tree becomes easier
;; act on a fully macro-expanded form

(defn prewalk-expr [f ast]
  (into ast (map (fn [[k v]] [k (f v)]) ast)))

(declare ast)
(declare parse-form)

(defn fn-ast [form])

(def ^dynamic *resolver* resolve)

(defn parse-do [locals & exprs]
  (cond
   (empty? exprs) nil
   (empty? (rest exprs)) (parse-form locals (first exprs))
   :else (let [parsed (map (partial parse-form locals) exprs)]
           {:type :do-expr
            :exprs parsed
            :result-type (:result-type (last parsed))
            })))

(defn parse-if [locals test then & else]
  (let [[test-ast then-ast else-ast]
          (map (partial parse-form locals) [test then (first else)])]
    {:type :if-expr
     :test test-ast
     :then then-ast
     :else else-ast
     :result-type (common-types (:result-type then-ast) (:result-type else-ast))
     }))

;; need to add bindings to locals

(defn make-map [f coll] (zipmap coll (map f coll)))

;; attach block of closures
;; at a function block move all locals to closures
;; when building locals track their type
;; each expression contains a set of used locals and used closures...

;; have a block to parse a single function
;; names get transformed into this expressions

;; if function name specified add it to locals block
(defn parse-single-fn [locals args & exprs]
  (let [arg-type (fn [sym] (or (:tag (meta sym)) Object))
        typed-args (map vector args (map arg-type args))
        body (apply parse-do (into locals typed-args) exprs)]
    {:type :single-fn-expr
     :args typed-args
     :body body
     :used (apply dissoc (:used body) args)}))

(defn parse-fn* [locals & params]
  (let [arrities ]

    )
  (letfn [(arg-type [sym] (or (:tag (meta sym)) Object))
          (single-arrity [[args & exprs]]
            {:args (vec (fn [sym] {:symbol sym :type (arg-type sym)}) params)
             :body (apply parse-do (merge locals (make-map param-type params)) exprs)})
          (bodies [params]
            (if (vector? (first params))
              (vector (single-arrity params))
              (vec (map single-arrity params))
              ))]
    (apply assoc
           {:type :fn-expr
            :result-type clojure.lang.Fn
            :used-locals #{}
            :used-closures #{}
             }
           (if (symbol? (first params))
             [:name (first params) :arrities (bodies (rest params))]
             [:arrities (bodies params)]))))

(defn parse-let* [locals bindings & exprs]
  (if (empty? bindings)
    (apply parse-do locals exprs)
    (let [[sym val & rst] bindings
          value (parse-form locals val)
          new-locals (assoc locals sym (:result-type value))
          body (apply parse-let* new-locals rst exprs)]
      {:type :let-one-expr
       :symbol sym
       :value value
       :expr body
       :result-type (:result-type body)})))

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
   'quote #(QuoteExpr. %&)
   ;; 'loop or 'loop*;; TODO
   })

(defn parse-seq [resolver locals f & params]
  (if (contains? parsers f)
    (apply (parsers f) resolver locals params)
    (let [r (partial parse-form resolver locals)]
      (invoke-expr (r f) (map r params)))))

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
