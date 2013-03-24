(ns clj-compiler.core)

(import '[clojure.asm ClassWriter])
(import '[clojure.asm ClassVisitor])
(import '[clojure.asm MethodVisitor])
(import '[clojure.asm Opcodes])
(import '[clojure.asm.commons Method])
(import '[clojure.asm.commons GeneratorAdapter])
(import '[clojure.asm Type])
(import '[clojure.asm Label])
(import '(java.lang.reflect Modifier))
(import 'java.io.FileOutputStream)

(def myfn (fn [x] (count x)))

(defn get-obj-methods [obj]
  (let [obj2methods (fn [obj] (map #(do (.setAccessible % true) %) (into [] (. (. obj getClass) getDeclaredMethods))))
        get-inst-methods (fn [fields] (filter #(not (Modifier/isStatic (.getModifiers %))) fields))
        method2ref (fn [field obj] (.get field obj))
        ]

    (obj2methods obj)))

(defn get-method-names [obj] (map #(.getName %) (get-obj-methods obj)))


(def ^{:dynamic true} *cw* nil)

(def mv-mock (proxy [clojure.asm.commons.EmptyVisitor ClassVisitor MethodVisitor] []
               (visitVarInsn [opcode index] (println "visitVarInsn" opcode index))
               (visitInsn [opcode] (println "visitInsn" opcode))
               (visitLdcInsn [value] (println "visitLdcInsn" value))
               (visitLabel [label] (println "visitLabel" label)
                 nil)
               (visitMethodInsn [opcode owner name desc] (println "visitMethodInsn" opcode owner name desc))
               (visitLocalVariable [name desc signature start end index] (println "visitLocalVariable" name desc signature start end index))
               (visitCode [] (println "visitCode"))
               (visitMethod [opcodes name desc signature exceptions] (println "visitMethod" opcodes name desc signature exceptions) this)
               (visit [version access classname signature super interfaces] (println "visit" version access classname signature super interfaces))
               (visitField [access name desc signature value] (println "visitField" access name desc signature value))
               (visitMaxs [a b] (println "visitMaxs" a b))
               (visitEnd [] (println "visitEnd"))
               (visitTypeInsn [opcode type] (println "visitTypeInsn" opcode type))
               (visitFieldInsn [opcode classname name type] (println "visitFieldInsn" opcode classname name type))


               ))

(defmacro with-classwriter [& body]
                `(binding [*cw* (new ClassWriter ClassWriter/COMPUTE_MAXS)]
                   ~@body))

(defmacro with-mockclasswriter [& body]
                `(binding [*cw* mv-mock]
                   ~@body))

(defn to-hex [byte-array] (map #(format "%x" %) (into [] byte-array)))



(defn write-bin-file [file byte-array]
(with-open [out (FileOutputStream. file)]
  (.write out byte-array)))


(defn mark [mv] (let [label (new Label)] (.visitLabel mv label) label))

(def interpret)

(def instruction-to-asm {
                         :var (fn [mv opcode index] (.visitVarInsn mv opcode index))
                         :instcode (fn [mv opcode] (.visitInsn mv opcode))
                         :ldc (fn [mv value] (.visitLdcInsn mv value))
                         :method (fn [mv opcode owner method-name method-desc] (.visitMethodInsn mv opcode owner method-name method-desc))
                         :mark (fn [mv] ;(println "mark")
                                 (mark mv))
                         :localvar (fn [mv name desc signature start end index] (.visitLocalVariable mv name desc signature start end index))

                         })
(defn compiler-interpreter [mv prevstate] {
                                           :let (fn [vars body]
                                                  ;(println "let" vars)
                                                  (let [state (into {} (map (fn [{var :var instruction :function}] [var (first (interpret mv [instruction] {}))]) vars))
                                                                                          state (merge state prevstate)]
                                                                            (interpret mv body state)
                                                                            state nil
                                                                  ))
                                 })
(defn label [s state] (get state s))


(defn resolve-labels [params state] (map (fn [v] (if (and (list? v) (= (first v) (symbol 'label))) (label (second v) state) v)) params))
(defn interpret [mv instructions state]
  ;(println "state=" state "instructions" instructions)
  (doall
   (for [{instruction :instruction compiler-instruction :compiler-instruction params :parameters} instructions]
     (if (nil? compiler-instruction)
       (apply (instruction-to-asm instruction) (cons mv (resolve-labels params state)))
       (apply ((compiler-interpreter mv state) compiler-instruction) params)))))

(defn generate-byte-code [ast]
  (let [{classname :classname superName :super fields :fields methods :methods} ast
        ] (with-classwriter
   (do
     (.visit *cw* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER Opcodes/ACC_FINAL) classname nil superName nil)
     ;(.visitSource *cw* "source" "smap")

     (doall (for [{name :name type :type} fields]
        (.visitField *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) name type nil nil)))

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
     (doall
      (for [{name :name desc :desc exceptions :exceptions instructions :instructions} methods]
        (let [mv (.visitMethod *cw* Opcodes/ACC_PUBLIC name desc nil exceptions)]
          (.visitCode mv)
          (interpret mv instructions {})
          (.visitMaxs mv 0 0)
          (.visitEnd mv))))
     (.visitEnd *cw*)

     (let [byte-code (.toByteArray *cw*)]
       (write-bin-file "./user$f2.class" byte-code)
       byte-code)))))

; very basic regression testing:
                                        ;(def original (into [] (generate-byte-code byte-code-ast)))
                                        ;(= (into [] original) (into [] (generate-byte-code byte-code-ast)))

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
                     :exceptions (into-array String '())
                     :instructions [{:compiler-instruction :let
                                     :parameters [[{:var "looplabel" :function {:instruction :mark}}]
                                                  [
                                                         {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ILOAD) 1]}
                                                         {:instruction :instcode :parameters [Opcodes/ACONST_NULL]}
                                                         {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ISTORE) 1]}
                                                         {:instruction :ldc :parameters [(new Long 2)]}
                                                         {:instruction :method :parameters [ Opcodes/INVOKESTATIC "clojure/lang/Numbers" "add" "(Ljava/lang/Object;J)Ljava/lang/Number;"]}
                                                         {:compiler-instruction :let
                                                          :parameters [[ {:var "end" :function {:instruction :mark}}]
                                                                       [{:instruction :localvar :parameters ["this" "Ljava/lang/Object;" nil '(label "looplabel") '(label "end") 0]}
                                                                        {:instruction :localvar :parameters ["x" "Ljava/lang/Object;" nil '(label "looplabel") '(label "end") 1]}
                                                                        ]]}
                                                         {:instruction :instcode :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/IRETURN)]}
                                                         ]
                                                  ]
                                     }
                                    ]
                     }

                   ]

          })

(def source-txt "(fn* ([x] (+ x 2)))")
(def classloader (.getContextClassLoader (Thread/currentThread)))

(defn load-test-function [] (.defineClass classloader "user$f2" (generate-byte-code nil) source-txt))

(defn run-test-function [] ((.newInstance (.loadClass classloader "user$f2")) 4))

;(pprint (to-hex (generate-byte-code byte-code-ast)))


;(load-test-function)
;(run-test-function)

;(first (into []  (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;")))
;(new Method "add" (Type/getReturnType "(Ljava/lang/Object;J)Ljava/lang/Number;") (Type/getArgumentTypes "(Ljava/lang/Object;J)Ljava/lang/Number;"))


(comment

;; This is just for test
(def test-instructions [{:compiler-instruction :let
                                     :parameters [[{:var "looplabel" :function {:instruction :mark}} {:var "looplabel2" :function {:instruction :mark}}]
                                                  [
                                                         {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ILOAD) 1]}
                                                         {:instruction :instcode :parameters [Opcodes/ACONST_NULL]}
                                                         {:instruction :var :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/ISTORE) 1]}
                                                         {:instruction :ldc :parameters [(new Long 2)]}
                                                         {:instruction :method :parameters [ Opcodes/INVOKESTATIC "clojure/lang/Numbers" "add" "(Ljava/lang/Object;J)Ljava/lang/Number;"]}
                                                         {:compiler-instruction :let
                                                          :parameters [[ {:var "end" :function {:instruction :mark}}]
                                                                       [{:instruction :localvar :parameters ["this" "Ljava/lang/Object;" nil '(label "looplabel") '(label "end") 0]}
                                                                        {:instruction :localvar :parameters ["x" "Ljava/lang/Object;" nil '(label "looplabel") '(label "end") 1]}
                                                                        ]]}
                                                         {:instruction :instcode :parameters [(.getOpcode (Type/getType java.lang.Object) Opcodes/IRETURN)]}
                                                         ]
                                                  ]
                                     }
                                    ])


;(first (interpret mv-mock test-instructions {}))



;(= (into [] original) (into [] (generate-byte-code byte-code-ast)))
;(pprint (generate-byte-code byte-code-ast))

;just for test
(let [{classname :classname superName :super fields :fields methods :methods} byte-code-ast] (doall
  (for [{name :name desc :desc exceptions :exceptions instructions :instructions} methods]
    (let [mv (.visitMethod mv-mock Opcodes/ACC_PUBLIC name desc nil exceptions)]
      (.visitCode mv)
      (interpret mv instructions {})))))

;(map (fn [v] (if (and (list? v) (= (first v) (symbol 'label))) (label (second v) {"looplabel" -99}) v)) ["this" "Ljava/lang/Object;" nil '(label "looplabel") '(label "end") 0])
)
