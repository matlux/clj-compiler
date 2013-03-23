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

(defrecord if-expr [test then else])

(defrecord do-expr [exprs])

(defrecord let-expr [bindings body])

(defrecord let-one-expr [sym val body])

(defrecord constructor [class-name])

(defrecord loop-expr [])

(defrecord recur-expr [])

(defrecord invoke-expr [f args])

;; step 1, fully macro-expand form
;; step 2, add do blocks where required
;; processing tree becomes easier
;; act on a fully macro-expanded form

(defn simplify [form]
  (match form
         ['do] nil
         ['do expr] (simplify expr)
         ['do & exprs] (cons do (map simplify exprs))
         ['let* [] & exprs] (simplify (cons 'do exprs))
         ['let* [sym val & bindings] & exprs] (list 'let* [sym (simplify val)] (simplify (apply 'let* (vec bindings) exprs)))
         form))

(defn ast [form]
  (match form
    ['do] nil
    ['do expr] (ast expr)
    ['do & exprs] (do-expr exprs)
    ['let* [] & exprs] (ast (cons 'do exprs))
    ['let* [sym val & bindings] & exprs] (let-one-expr sym (ast val) (ast (apply list 'let* bindings exprs)))

    ['if test then]      (if-expr (ast test) (ast then) nil)
    ['if test then else] (if-expr (ast test) (ast then) (ast else))
    form
    ))

;; define a function for construction? - multiple indirection, maybe slower
