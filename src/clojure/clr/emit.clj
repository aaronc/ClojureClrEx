(ns clojure.clr.emit
  (:import
   [System.Reflection TypeAttributes PropertyAttributes MethodAttributes FieldAttributes EventAttributes CallingConventions MethodImplAttributes BindingFlags AssemblyName Assembly]
   [System.Reflection.Emit TypeBuilder PropertyBuilder MethodBuilder ILGenerator AssemblyBuilder AssemblyBuilderAccess OpCodes EnumBuilder CustomAttributeBuilder]
   [System.Runtime.InteropServices CallingConvention CharSet Marshal]
   [clojure.lang Compiler]
   [clojure.lang.CljCompiler.Ast GenContext]))

(def ^:private _save-id (atom 0))

(defn- enum-keys [enum-type keys]
  (apply enum-or (map #(enum-val enum-type (name %)) keys)))

(defn- enum-key [enum-type key]
  (enum-val enum-type (name key)))

(def ^:dynamic *gen-context* nil)

(let [ctxt-field (.GetField Compiler "CompilerContextVar" (enum-keys BindingFlags [:Static :NonPublic :Public]))]
  (def cur-gen-context-var (.GetValue ctxt-field nil)))

(defn cur-gen-context []
  (or *gen-context* @cur-gen-context-var Compiler/EvalContext))

(defn make-typed-array [type elems]
  (let [n (count elems)
        res (make-array type n)]
    (doall (map-indexed #(aset res % %2) elems))
    res))

(defn type-array [& elems]
  (make-typed-array Type elems))

(defn obj-array [& elems]
  (make-typed-array Object elems))

(comment (defn wrap-gen-context [gen-func]
           (let [ctxt (GenContext/CreateWithInternalAssembly (str "__data__gen__" (swap! _save-id inc)) false)]
             (gen-func ctxt))))

(defn clr-assembly* [name access]
  (AssemblyBuilder/DefineDynamicAssembly
    (AssemblyName. name)
    (enum-key AssemblyBuilderAccess access)))

(defn clr-module*
  ([^AssemblyBuilder asm 
    ]))

(defn clr-type*
  ([^GenContext gen-ctxt name attrs parent ifaces]
     (.DefineType (.ModuleBuilder gen-ctxt)
                  name (enum-keys TypeAttributes attrs)
                  parent (make-typed-array Type ifaces)))
  ([^GenContext gen-ctxt name]
     (clr-type* gen-ctxt name [:Public] nil nil)))

(defn clr-constructor*
  [^TypeBuilder tb attrs calling-convs param-types]
  (.DefineConstructor tb (enum-keys MethodAttributes attrs)
                      (enum-keys CallingConventions calling-convs)
                      (make-typed-array Type param-types)))

(defn clr-field*
  [^TypeBuilder tb name type attrs]
  (.DefineField tb name type (enum-keys FieldAttributes attrs)))

(defn clr-property*
  ([^TypeBuilder tb name type attrs param-types]
     (.DefineProperty tb name (enum-keys PropertyAttributes attrs) type param-types))
  ([^TypeBuilder tb name type]
     (clr-property* tb name type :None nil)))

(def ^:private get-set-attrs [:Public :SpecialName :HideBySig])


(def ^:dynamic *type-builder* nil)

(def ^:dynamic *property-builder* nil)

(def ^:dynamic *ilgen* nil)

(def ^:dynamic *method-builder* nil)

(defn clr-method*
  ([^TypeBuilder tb name attrs ret-type param-types]
     (.DefineMethod tb name (enum-keys MethodAttributes attrs)
                  ret-type (make-typed-array Type param-types))))

(defn clr-getter*
  [^TypeBuilder tb ^PropertyBuilder prop]
  (clr-method* tb (str "get_" (.Name prop))
                 get-set-attrs (.PropertyType prop) nil))

(defn clr-setter*
  [^TypeBuilder tb ^PropertyBuilder prop]
  (clr-method* tb (str "set_" (.Name prop))
               get-set-attrs nil [(.PropertyType prop)]))

(defn clr-event*
  ([^TypeBuilder tb name attrs type]
     (.DefineEvent tb name (enum-keys EventAttributes attrs) type)))

(defn clr-pinvoke*
  ([^TypeBuilder tb name dll-name attrs calling-convs ret-type param-types native-call-conv native-char-set]
     (let [^MethodBuilder mb (.DefinePInvokeMethod tb name dll-name
                                                   (enum-keys MethodAttributes attrs)
                                                   (enum-keys CallingConventions calling-convs)
                                                   ret-type
                                                   (make-typed-array Type param-types)
                                                   (enum-key CallingConvention native-call-conv)
                                                   (enum-key CharSet native-char-set))]
       (.SetImplementationFlags mb (enum-or (.GetMethodImplementationFlags mb) MethodImplAttributes/PreserveSig))
       mb))
  ([^TypeBuilder tb name dll-name attrs ret-type param-types]
     (clr-pinvoke* tb name dll-name attrs [:Standard] ret-type param-types :Winapi :Ansi))
  ([^TypeBuilder tb name dll-name ret-type param-types]
     (clr-pinvoke* tb name dll-name [:Public :Static :PinvokeImpl] ret-type param-types)))

(defn- set-dg-impl-flags [mb]
  (.SetImplementationFlags mb (enum-keys MethodImplAttributes [:Runtime :Managed])))

(defn clr-delegate* [ret params]
  (let [gen-ctxt (cur-gen-context)
        tb (clr-type* gen-ctxt (str "gendg__" (swap! _save-id inc))
                      [:Class :Public :Sealed :AnsiClass :AutoClass]
                      System.MulticastDelegate nil)
        cb (clr-constructor* tb [:RTSpecialName :HideBySig :Public] [:Standard] [Object IntPtr])
        mb (clr-method* tb "Invoke" [:Public :HideBySig :NewSlot :Virtual] ret params)]
    (set-dg-impl-flags cb)
    (set-dg-impl-flags mb)
    (.CreateType tb)))

(defn clr-enum*
  ([gen-ctxt name attrs ^Type underlying-type flags? literals]
     (let [^EnumBuilder enum-builder (.DefineEnum (.ModuleBuilder gen-ctxt)
                                     name
                                     (enum-keys TypeAttributes attrs)
                                     underlying-type)]
       (when flags?
         (.SetCustomAttribute enum-builder
                              (CustomAttributeBuilder.
                               (.GetConstructor FlagsAttribute
                                                Type/EmptyTypes)
                               (obj-array))))
       (doseq [[name value] literals]
         (.DefineLiteral (name name) value))
       (.CreateType enum-builder))))

(defn- get-opcode [opcode]
  (.GetValue (.GetField OpCodes (name opcode)) nil))

(defn op
  ([opcode]
     (.Emit *ilgen* (get-opcode opcode)))
  ([opcode arg]
     (.Emit *ilgen* (get-opcode opcode) arg)))

(defmacro get-member [])

(defmacro get-local [])

(defn method*
  [name metadata params body-func])

(defmacro method [])

(defmacro property [])

;;(defmacro event)

(defn defclrtype* [name metadata body-fn])

(defmacro defclrtype [name & body]
  `(defclrtype* ~(name name) ~(meta name)
     (fn ~(symbol (str (name name) "-body-fn"))
       ~@body)))
