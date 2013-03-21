
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

(let [superName "clojure/lang/AFunction"] (with-classwriter
   (do
     (.visit *cw* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER Opcodes/ACC_FINAL) "user$f2" nil superName nil)
     (let [clinitgen (new GeneratorAdapter (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                          (Method/getMethod "void <clinit> ()")
                          nil
                          nil
                          *cw*)]
       (.returnValue clinitgen)
       (.endMethod clinitgen))
     (let [ctorgen (new GeneratorAdapter Opcodes/ACC_PUBLIC
                        (Method. "<init>" Type/VOID_TYPE (make-array Type 0))
                        nil
                        nil
                        *cw*)
           start (.newLabel ctorgen)
           end (.newLabel ctorgen)
           ]
       ;(.visitCode ctorgen)
       (.visitLabel ctorgen start)
       (.loadThis ctorgen)
       (.invokeConstructor ctorgen (Type/getObjectType superName) (Method/getMethod "void <init>()"))
       ;(.visitCode ctorgen end)
       (.returnValue ctorgen)
       (.endMethod ctorgen))
     (let [
           object-type (Type/getType java.lang.Object)
           gen (new GeneratorAdapter Opcodes/ACC_PUBLIC
                        (Method. "invoke" object-type (into-array Type (list object-type)))
                        nil
                        nil
                        *cw*)
           looplabel (.mark gen)
           method-desc "(Ljava/lang/Object;J)Ljava/lang/Number;"
           ]

       ;(.visitCode gen)
       (.loadArg gen 0)
       (.visitInsn gen Opcodes/ACONST_NULL)
       (.storeArg gen 0)
       (.push gen 2)
       (.invokeStatic gen (Type/getType clojure.lang.Numbers) (new Method "add" (Type/getReturnType method-desc) (Type/getArgumentTypes method-desc)))
       (let [end (.mark gen)]
         (.visitLocalVariable gen "x" "Ljava/lang/Object;" nil looplabel end 1))

       (.returnValue gen)
       (.endMethod gen))
     (.visitEnd *cw*)
     ;(write-bin-file "./user$f2.class" (.toByteArray *cw*))
     (.toByteArray *cw*))))


;(first (into []  (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;")))
;(new Method "add" (Type/getReturnType "(Ljava/lang/Object;J)Ljava/lang/Number;") (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;"))

(comment

  )
