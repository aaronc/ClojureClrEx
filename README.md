ClojureClrEx
============

A small set of  libraries for ClojureCLR that I haven't taken the time to put into a separate repositories yet.
Included is a port of clojure.tools.logging.  Also, there is:

* clojure.clr.emit - Wrappers around System.Reflection.Emit for dynamically generating .NET byte code.
* clojure.clr.pinvoke - Provides dllimport and dllimports macros for dynamic P/Invoke. Ex:

```clojure
(dllimports "kernel32.dll"
            (LoadLibrary IntPtr [String])
            (GetProcAddress IntPtr [IntPtr String])
            (FreeLibrary nil [IntPtr]))
```

* clojure.clr.socketrepl - A simple TCP socket repl with very basic functionality.
