(ns clojure.clr.pinvoke
  (:use [clojure.clr.emit]))

(def ^:private _save-id (atom 0))

;; (defn dllimport* [dll-name fn-name ret-type param-types]
;;   (defclrtype* (clojure.lang.Compiler/munge
;;                 (str dll-name "/" fn-name (swap! _save-id inc)))
;;     {:extends clojure.lang.AFn}
;;     (fn dll-import-def-fn []
;;       (method invoke))))

(defn dllimport* [dll-name fn-name ret-type param-types]
  (let [gen-ctxt (cur-gen-context)
        tb (clr-type* gen-ctxt (str "pinvoke__" (swap! _save-id inc)))
        mname (str fn-name)
        mb (clr-pinvoke* tb mname dll-name ret-type (into-array Type param-types))
        ptype (.CreateType tb)
        invoke-method (.GetMethod ptype mname)]
    (fn pinvoke-wrapper [& args] (.Invoke invoke-method nil (into-array Object args)))))

(defmacro dllimport [dll fn-name ret params]
  `(def ~fn-name
     (dllimport* ~dll ~(name fn-name) ~ret ~params)))

(defmacro dllimports [dll & imports]
  `(do ~@(for [imp imports]
         (let [imp (into [dll] imp)]
           `(dllimport ~@imp)))))

; Test code
(comment (dllimports "kernel32.dll"
                     (LoadLibrary IntPtr [String])
                     (GetProcAddress IntPtr [IntPtr String])
                     (FreeLibrary nil [IntPtr])))
;(def k32 (LoadLibrary "kernel32.dll"))
;(def beep (GetProcAddress k32 "Beep"))
;(def beep-dg (clr-delegate* Boolean [UInt32 UInt32]))
;(let [pbeep (Marshal/GetDelegateForFunctionPointer beep beep-dg)]
;  (defn dbeep [freq dur] (.Invoke pbeep freq dur))
; See http://msdn.microsoft.com/en-us/library/0sfb502t(v=vs.80).aspx
