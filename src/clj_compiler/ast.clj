(ns clj-compiler.ast
  (:use [clojure.core.match :only (match)]))

;; steps
;; special type for constructor
;; translate locals so each has a type + slot index?
;; or just unique name?

;; can calculate own max-stack frame
;; when we exit block of code stack frames dissappear, slots become empty

;; feed bytecode generator indices and types for local vars
;; can work out max stack size

;; can feed asm raw byte-codes and it computes calculated values?

;; step 1, normalize all multi-expression statements into do blocks

;; each expression is tagged with a type, primitive

(defrecord IfExpr [test then else])

(defrecord DoExpr [exprs])

(defrecord LetOneExpr [sym val body])

(defrecord constructor [class-name])

(defrecord loop-expr [])

(defrecord recur-expr [])

(defrecord LiteralExpr [literal])

(defrecord FnBody [params body])

(defrecord FnExpr [name? arrities unbounded?])

(defrecord InvokeExpr [f args])

(defn let-one-expr [sym val body]
  (assoc (LetOneExpr. sym val body)
    :expr-type :let-one-expr))

(defn literal-expr [literal]
  (assoc (LiteralExpr. literal)
    :expr-type :literal-expr
    :class (type literal)))

;; step 1, fully macro-expand form
;; step 2, add do blocks where required
;; processing tree becomes easier
;; act on a fully macro-expanded form

(declare ast)

(defn seq-ast [[f & params]]
  (condp = f
    'do (if-let [[f & r] (not-empty params)]
          (if (empty? r) (ast f) (DoExpr. (map ast params))))

    'if (let [[test then else] (map ast params)]
          (IfExpr. test then else))

    'let* (let [[[sym val & r :as bindings] & body] params]
            (if (empty? bindings)
             (ast (cons 'do body))
             (let-one-expr sym (ast val) (ast (apply list 'let* (vec r) body)))))

    (InvokeExpr. (ast f) (map ast params))))

(defn ast [form]
  (let [m (macroexpand form)]
    (cond
     (seq? m) (seq-ast m)
     (coll? m) (into (empty m) (map ast m))
     :else (literal-expr m)
     )))

;; map through all types

;; define a function for construction? - multiple indirection, maybe slower
