
(ns clj-compiler.core)

(def myfn (fn [x] (count x)))

(def ast '())

(defrecord if-expr [test then else])

(defrecord do-expr [& exprs])

(defrecord fn-expr [params body])

(defrecord let-one-expr [symbol value-expr body-exor])

(defn ast [clojure-form]) => nested structure of defrecords


(let [a 1
      b 2
      c 3]
  expr)

(let-one-expr a 1
              (let-one-expr b 2
                            ))
(let [a 1]
  (let [b 2]
    (let c 3)
    expr))
