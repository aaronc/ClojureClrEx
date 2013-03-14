(ns clojure.clr.ildasm
  (:import
   [Mono.Reflection Disassembler]
   [System.Reflection BindingFlags
    MethodBase])
  (:require
   [clojure.string :as str]))

(defn- il-label [inst]
  (str "IL_" (.ToString (.Offset inst) "x4")))

(defn- decl-type->str [mi]
  (str "[" (.DeclaringType mi) "]"))

(defn- method-info->str [mi]
  (str mi " " (decl-type->str mi)))

(defn- field-info->str [fi]
  (str fi " " (decl-type->str fi)))

(defn- operand->str [operand operand-type]
  (case (str operand-type)
    "ShortInlineBrTarget"
    (il-label operand)
    "InlineBrTarget"
    (il-label operand)
    "InlineSwitch"
    (str/join "," (map il-label operand))
    "InlineString"
    (str "\"" operand "\"")
    "InlineField"
    (field-info->str operand)
    "InlineMethod"
    (method-info->str operand)
    operand))

(defn- show-instruction [inst]
  (let [opcode (.OpCode inst)
        operand (.Operand inst)]
    (println
     (str (il-label inst)
          ": " (. opcode Name)
          (when operand
            (str " "
                 (operand->str
                  operand
                  (.OperandType opcode))))))))

(defn- show-method-il [method]
  (println "Method:" (str method))
  (doseq [instruction (Disassembler/GetInstructions method)]
    (show-instruction instruction))
  (println))

(defn- to-type [type-or-obj]
  (if (instance? Type type-or-obj)
    type-or-obj
    (class type-or-obj)))

(defn- field-decl->str [fi]
  (let [static (.IsStatic fi)
        value (when static (.GetValue fi nil))]
    (str (when static "static ") (str fi)
         (when value (str " = " value)) ";")))

(defn- show-obj-or-type-il [obj-or-type]
  (let [cls (to-type obj-or-type)
        binding-flags (enum-or BindingFlags/DeclaredOnly
                               BindingFlags/Public
                               BindingFlags/NonPublic
                               BindingFlags/Instance
                               BindingFlags/Static)
        methods (.GetMethods cls binding-flags)
        fields (.GetFields cls binding-flags)]
    (println "Type:" cls)
    (println)
    (when-not (empty? fields)
      (println "Fields:")
      (doseq [f fields]
        (println (field-decl->str f)))
      (println))
    (doseq [m (reverse methods)]
      (show-method-il m))))

(defn show-il
  ([obj-type-or-method]
     (if (instance? MethodBase obj-type-or-method)
       (show-method-il obj-type-or-method)
       (show-obj-or-type-il obj-type-or-method)))
  ([type-or-obj method-name]
     (show-method-il (.GetMethod (to-type type-or-obj) method-name)))
  ([type-or-obj method-name arg-types]
     (show-method-il (.GetMethod (to-type type-or-obj) method-name
                                 (into-array Type arg-types)))))
