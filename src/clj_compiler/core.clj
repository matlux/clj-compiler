
(ns clj-compiler.core)

(import '[clojure.asm ClassWriter])
(import '[clojure.asm Opcodes])
(import '[clojure.asm.commons Method])
(import '[clojure.asm.commons GeneratorAdapter])
(import '[clojure.asm Type])

(import 'java.io.FileOutputStream)

(def myfn (fn [x] (count x)))

(def ast '())

;(defrecord if-expr [test then else])

;(defrecord do-expr [& exprs])

;(defrecord fn-expr [params body])

;(defrecord let-one-expr [symbol value-expr body-exor])

;(defn ast [clojure-form]) ; => nested structure of defrecords


;(let [a 1
;      b 2
;      c 3]
;  expr)

;(let-one-expr a 1
;(let-one-expr b 2
;))

(comment
  (let [a 1]
    (let [b 2]
      (let c 3)
      expr))
  )

(def ^{:dynamic true} *cw* nil)


(defmacro with-classwriter [& body]
                `(binding [*cw* (new ClassWriter ClassWriter/COMPUTE_MAXS)]
                   ~@body))

(defn to-hex [byte-array] (map #(format "%x" %) (into [] byte-array)))


(defn write-bin-file [file byte-array]
(with-open [out (FileOutputStream. file)]
  (.write out byte-array)))

(with-classwriter
  (do
    (.visit *cw* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER Opcodes/ACC_FINAL) "user$f2" nil "clojure/lang/AFunction" nil)
    (let [clinitgen (new GeneratorAdapter (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                         (Method/getMethod "void <clinit> ()")
                         nil
                         nil
                         *cw*)]
      (.returnValue clinitgen)
      (.endMethod clinitgen))

    (.visitEnd *cw*)
    (write-bin-file "./user$f2.class" (.toByteArray *cw*))))

(comment
(let [ctorgen (new GeneratorAdapter Opcodes/ACC_PUBLIC
                         (Method/getMethod "<init>")
                         nil
                         nil
                         *cw*)]
      (.returnValue ctorgen)
      (.endMethod ctorgen))
  )
