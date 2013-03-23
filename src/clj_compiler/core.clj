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
     ;(.visitSource *cw* "source" "smap")
     (.visitField *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) "const__0" "Lclojure/lang/Var;" nil nil)
     (.visitField *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) "const__1" "Ljava/lang/Object;" nil nil)

     ;; class initialisation
     (let [
           mv (.visitMethod *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "<clinit>" "()V" nil nil)]
       (.visitCode mv)
       ;(.visitLineNumber line (mark mv))
       (.visitLdcInsn mv "clojure.core")
       (.visitLdcInsn mv "+")
       (.visitMethodInsn mv Opcodes/INVOKESTATIC "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;")
       (.visitTypeInsn mv Opcodes/CHECKCAST "clojure/lang/Var")
       (.visitFieldInsn mv Opcodes/PUTSTATIC "user$f2" "const__0" "Lclojure/lang/Var;")
       (.visitLdcInsn mv (new Long 2))
       (.visitMethodInsn mv Opcodes/INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;")
       (.visitFieldInsn mv Opcodes/PUTSTATIC "user$f2" "const__1" "Ljava/lang/Object;")
       (.visitInsn mv (.getOpcode (Type/getType "V") Opcodes/IRETURN))
       (.visitMaxs mv 0 0)
       (.visitEnd mv))

     ;; constructor
     (let [
           mv (.visitMethod *cw* Opcodes/ACC_PUBLIC "<init>" "()V" nil nil)]
       (.visitCode mv)
       ;(.visitLineNumber line (mark mv))
       ;(.visitLabel)
       (.visitVarInsn mv Opcodes/ALOAD 0)
       (.visitMethodInsn mv Opcodes/INVOKESPECIAL "clojure/lang/AFunction" "<init>" "()V")
       (.visitInsn mv (.getOpcode (Type/getType "V") Opcodes/IRETURN))
       (.visitMaxs mv 0 0)
       (.visitEnd mv))

     ;; invoke method (function code)
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
       (.visitLdcInsn mv (new Long 2))
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

(def byte-code-ast {
          :classname "user$f2"
          :super "clojure/lang/AFunction"
          :fields [
                   {:name "const__0" :type "Lclojure/lang/Var;"}
                   {:name "const__1" :type "Ljava/lang/Object;"}
                   ]
          :clinit [
                   {:instruction :ldc :parameters ["clojure.core"]}
                   {:instruction :ldc :parameters ["+"]}
                   {:instruction :method :parameters [Opcodes/INVOKESTATIC "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]}
                   {:instruction :type :parameters [Opcodes/CHECKCAST "clojure/lang/Var"]}
                   {:instruction :field :parameters [Opcodes/PUTSTATIC "user$f2" "const__0" "Lclojure/lang/Var;"]}
                   {:instruction :ldc :parameters [(new Long 2)]}
                   {:instruction :method :parameters [Opcodes/INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;"]}
                   {:instruction :field :parameters [Opcodes/PUTSTATIC "user$f2" "const__1" "Ljava/lang/Object;"]}
                   {:instruction :instcode :parameters [(.getOpcode (Type/getType "V") Opcodes/IRETURN)]}
                   ]
          :init [
                 {:instruction :var :parameters [Opcodes/ALOAD 0]}
                 {:instruction :method :parameters [Opcodes/INVOKESPECIAL "clojure/lang/AFunction" "<init>" "()V"]}
                 {:instruction :instcode :parameters [(.getOpcode (Type/getType "V") Opcodes/IRETURN)]}
                 ]
          :methods [
                    {:name "invoke"
                     :desc "(Ljava/lang/Object;)Ljava/lang/Object;"
                     :parameters (into-array String '())
                     :instructions [
                                    {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ILOAD) 1]}
                                    {:instruction :instcode :parameters [Opcodes/ACONST_NULL]}
                                    {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ISTORE) 1]}
                                    {:instruction :ldc :parameters [(new Long 2)]}
                                    {:instruction :method :parameters [ Opcodes/INVOKESTATIC "clojure/lang/Numbers" "add" "(Ljava/lang/Object;J)Ljava/lang/Number;"]}
                                    {:instruction :instcode :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/IRETURN)]}
                                    ]
                     }

                   ]

          })

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
