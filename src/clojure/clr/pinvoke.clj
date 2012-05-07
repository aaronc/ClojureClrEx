(ns clojure.clr.pinvoke
  (:use [clojure.clr.emit]))

(def ^:private _save-id (atom 0))

(defmacro dllimport [dll name ret params]
  (let [gen-ctxt (cur-gen-context)
        tb (clr-type* gen-ctxt (str "pinvoke__" (swap! _save-id inc)))
        mname (str name)
        mb (clr-pinvoke* tb mname dll (eval ret) (eval params))
        ptype (.CreateType tb)]
    `(clojure.core/defn ~name [& args#]
       (.Invoke (.GetMethod ~ptype ~mname) nil (clojure.core/to-array args#)))))

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