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
               (toByteArray [] "BYTE_CODE_EXAMPLE")

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

(defn instruction-to-asm [state] {
                         :var (fn [mv opcode index] (.visitVarInsn mv opcode index) state)
                         :instcode (fn [mv opcode] (.visitInsn mv opcode) state)
                         :ldc (fn [mv value] (.visitLdcInsn mv value) state)
                         :method (fn [mv opcode owner method-name method-desc] (.visitMethodInsn mv opcode owner method-name method-desc) state)
                         :mark (fn [mv] ;(println "mark")
                                 (mark mv) state)
                         :localvar (fn [mv name desc signature start end index] (.visitLocalVariable mv name desc signature start end index) state)
                         :type (fn [mv opcode type] (.visitTypeInsn mv opcode type) state)
                         :field (fn [mv opcode owner name desc] (.visitFieldInsn mv opcode owner name desc) state)

                         })
(defn compiler-interpreter [mv state] {

                                           :label (fn [labelname]
                                                    (let [found (get state labelname)]
                                                      (if found (do (.visitLabel mv found) state) (let [newlabel (mark mv)] (merge state {labelname newlabel})))))
                                 })

(defn label [s state] (get state s))


(defn resolve-labels [params state] (map (fn [k] (get state k k)) params))

(defn compiler-instruction? [instruction] (= instruction :label))

(defn interpret-one [mv]
  (fn [state [instruction & args]]
    ;;(println instruction state (cons mv (resolve-labels args state)))
    (if (compiler-instruction? instruction)
      (apply ((compiler-interpreter mv state) instruction) args)
      (apply ((instruction-to-asm state) instruction) (cons mv (resolve-labels args state)))
        )))


;;(instruction-to-asm :localvar)
;;(= (into [] original) (into [] (generate-byte-code byte-code-ast)))

(defn interpret [mv instructions]
  ;(println "mv=" mv "instructions" instructions)
  (reduce (interpret-one mv) {} instructions))



(defn generate-method-byte-code [cw access name desc signature exceptions instructions]
  (let [mv (.visitMethod cw access name desc signature exceptions)]
    (.visitCode mv)
    (interpret mv instructions)
    (.visitMaxs mv 0 0)
    (.visitEnd mv)))

(defn generate-byte-code [ast]
  (let [{classname :classname superName :super fields :fields clinit :clinit init :init methods :methods} ast]
   (with-classwriter
    (do
     (.visit *cw* Opcodes/V1_5 (+ Opcodes/ACC_PUBLIC Opcodes/ACC_SUPER Opcodes/ACC_FINAL) classname nil superName nil)
     ;(.visitSource *cw* "source" "smap")

     (doall (for [{name :name type :type} fields]
        (.visitField *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC Opcodes/ACC_FINAL) name type nil nil)))

     ;; class initialisation
     (generate-method-byte-code *cw* (+ Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) "<clinit>" "()V" nil nil clinit)

     ;; constructor
     (generate-method-byte-code *cw* Opcodes/ACC_PUBLIC "<init>" "()V" nil nil init)


     ;; invoke method (function code)
     (doall
      (for [{name :name desc :desc exceptions :exceptions instructions :instructions} methods]
        (generate-method-byte-code *cw* Opcodes/ACC_PUBLIC name desc nil exceptions instructions)
        ))
     (.visitEnd *cw*)

     (let [byte-code (.toByteArray *cw*)]
       (write-bin-file "./user$f2.class" byte-code)
       byte-code)))))

; very basic regression testing:
                                        ;(def original (into [] (generate-byte-code byte-code-ast)))
;;(= (into [] original) (into [] (generate-byte-code byte-code-ast)))


(def byte-code-ast {
          :classname "user$f2"
          :super "clojure/lang/AFunction"
          :fields [
                   {:name "const__0" :type "Lclojure/lang/Var;"}
                   {:name "const__1" :type "Ljava/lang/Object;"}
                   ]
          :clinit [
                   [:ldc "clojure.core"]
                   [:ldc "+"]
                   [:method Opcodes/INVOKESTATIC "clojure/lang/RT" "var" "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"]
                   [:type Opcodes/CHECKCAST "clojure/lang/Var"]
                   [:field Opcodes/PUTSTATIC "user$f2" "const__0" "Lclojure/lang/Var;"]
                   [:ldc (new Long 2)]
                   [:method Opcodes/INVOKESTATIC "java/lang/Long" "valueOf" "(J)Ljava/lang/Long;"]
                   [:field Opcodes/PUTSTATIC "user$f2" "const__1" "Ljava/lang/Object;"]
                   [:instcode (.getOpcode (Type/getType "V") Opcodes/IRETURN)]
                   ]
          :init [
                 [:var Opcodes/ALOAD 0]
                 [:method Opcodes/INVOKESPECIAL "clojure/lang/AFunction" "<init>" "()V"]
                 [:instcode (.getOpcode (Type/getType "V") Opcodes/IRETURN)]
                 ]
          :methods [
                    {:name "invoke"
                     :desc "(Ljava/lang/Object;)Ljava/lang/Object;"
                     :exceptions (into-array String '())
                     :instructions [
                                    [:label "@looplabel"]
                                    [:var (.getOpcode (Type/getType java.lang.Object) Opcodes/ILOAD) 1]
                                    [:instcode Opcodes/ACONST_NULL]
                                    [:var (.getOpcode (Type/getType java.lang.Object) Opcodes/ISTORE) 1]
                                    [:ldc (new Long 2)]
                                    [:method Opcodes/INVOKESTATIC "clojure/lang/Numbers" "add" "(Ljava/lang/Object;J)Ljava/lang/Number;"]
                                    [:label "@end"]
                                    [:localvar "this" "Ljava/lang/Object;" nil "@looplabel" "@end" 0]
                                    [:localvar "x" "Ljava/lang/Object;" nil "@looplabel" "@end" 1]
                                    [:instcode (.getOpcode (Type/getType java.lang.Object) Opcodes/IRETURN)]
                                    ]

                     }

                   ]

                    })

(into [] (generate-byte-code byte-code-ast))

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
