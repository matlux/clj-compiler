
(ns clj-compiler.core)

(import '[clojure.asm ClassWriter])
(import '[clojure.asm Opcodes])
(import '[clojure.asm.commons Method])
(import '[clojure.asm.commons GeneratorAdapter])
(import '[clojure.asm Type])
(import '[clojure.asm Label])

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

(defn mark [mv] (let [label (new Label)] (.visitLabel mv label) label))

(defn generate-byte-code [ast]
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
           add-method-desc "(Ljava/lang/Object;J)Ljava/lang/Number;"
           mv (.visitMethod *cw* Opcodes/ACC_PUBLIC "invoke" "(Ljava/lang/Object;)Ljava/lang/Object;" nil (into-array String '()))
           _ (.visitCode mv)
           looplabel (mark mv)
           ]

       (.visitVarInsn mv (.getOpcode object-type Opcodes/ILOAD) 1)
       (.visitInsn mv Opcodes/ACONST_NULL)
       (.visitVarInsn mv (.getOpcode object-type Opcodes/ISTORE) 1)
       (.visitLdcInsn mv (new Long 3))
       (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/Numbers" "add" add-method-desc)
       (let [end (mark mv)]
         (.visitLocalVariable mv "this" "Ljava/lang/Object;" nil looplabel end 0)
         (.visitLocalVariable mv "x" "Ljava/lang/Object;" nil looplabel end 1))
       (.visitInsn mv (.getOpcode object-type Opcodes/IRETURN))
       (.visitMaxs mv 0 0)
       (.visitEnd mv))
     (.visitEnd *cw*)

     (let [byte-code (.toByteArray *cw*)]
       (write-bin-file "./user$f2.class" byte-code)
       byte-code)))))

(def source-txt "(fn* ([x] (+ x 2)))")
(def classloader (.getContextClassLoader (Thread/currentThread)))

(defn load-test-function [] (.defineClass classloader "user$f2" (generate-byte-code nil) source-txt))

(defn run-test-function [] ((.newInstance (.loadClass classloader "user$f2")) 4))

;(load-test-function)
;(run-test-function)

;(first (into []  (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;")))
;(new Method "add" (Type/getReturnType "(Ljava/lang/Object;J)Ljava/lang/Number;") (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;"))

(comment

  )
