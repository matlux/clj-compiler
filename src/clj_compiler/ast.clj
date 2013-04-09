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

;; output must be syntactically valid

(defn to-bytecode
  "Return a map of sym -> lambda forms to compile"
  [ast]
  )

(defn fn-exprs [form]

  )

(defn map-vals [f m]
  (apply assoc m (interleave (keys m) (map f (vals m)))))

(defprotocol Expr
  (result-type [this])
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

(defrecord LocalSymbolExpr [symbol]
  Expr
  #_(unbound [{:keys [symbol]} local-symbols]
    (if (contains? local-symbols) #{} #{symbol})))

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

;; step 1, fully macro-expand form
;; step 2, add do blocks where required
;; processing tree becomes easier
;; act on a fully macro-expanded form

(declare ast)
(declare parse-form)

(defn fn-ast [form]

  )

(defn parse-do [& exprs]
  (cond
   (empty? exprs) nil
   (empty? (rest exprs)) (parse-form (first exprs))
   :else (do-expr (map parse-form exprs))))

(defn let-ast [[bindings body] lexical-symbols]
  (if (empty? bindings)
    (ast (cons 'do body) lexical-symbols)
    (let [[sym val & others] bindings
          expr (let-ast [others body] (conj lexical-symbols sym))]
      (let-one-expr sym (ast val lexical-symbols) expr))))

(defn parse-if [test then & else]
  (if-expr (parse-form test) (parse-form then) (parse-form (first else))))

(defn parse-fn* [& params]
  (letfn [(with-do [args & bodies]
            (list args (apply parse-do bodies)))

          (bodies [params]
            (if (vector? (first params))
              (list (apply with-do params))
              (map (partial apply with-do) params)
              ))]

    (if (symbol? (first params))
      (fn-expr (first params) (bodies (rest params)))
      (fn-expr nil (bodies params)))))

#_(defn multiple? [coll] (not-empty? (rest coll)))

#_(def require-do? multiple?)



(defn parse-let* [bindings & exprs]
  (if (empty? bindings)
    (apply parse-do exprs)
    (let [[sym val & rst] bindings]
      (LetOneExpr. sym (parse-form val)
        (apply parse-let* rst exprs)))))

(def parsers
  {'do parse-do
   'if parse-if
   'fn* parse-fn*
   'let* parse-let*
   'quote #(QuoteExpr. %&)
   ;; 'loop or 'loop*;; TODO
   })

(defn parse-seq [f & params]
  (if (contains? parsers f)
    (apply (parsers f) params)
    (invoke-expr (parse-form f) (map parse-form params))))

(defn parse-form [form]
  (let [m (macroexpand form)]
    (cond
     (and (seq? m) (not-empty m)) (apply parse-seq m)
     (coll? m) (clojure.walk/walk parse-form identity m)
     :else m)))

#_(defn symbol-ast [symbol lexical-symbols]
  (cond
   (symbol-ns m) m ;; TODO
   (lexical-symbols m) (LocalSymbolExpr. m)

   ;; perform the lookup for the global symbol?
   :else               (GlobalSymbolExpr. m)))

;; step 1 build simple AST
;; step 2 resolve symbols
;; step 3 add types

;; a lexical symbol resolves to an expression - with a type
;; knowing the type at point of usage is important
;; as we progress keep a map if symbol->type
;; direct links can be typed, vars are only objects

(defn resolve-symbols
  "Run through the AST resolving local symbols to Global, Var, Local or Unresolvable"
  [ast lexical-symbols]

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
  [ast scoped]

  )

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
