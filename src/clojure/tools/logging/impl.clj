;; Copyright (c) Alex Taggart. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns ^{:author "Alex Taggart"
      :doc "Protocols used to allow access to logging implementations.
            This namespace only need be used by those providing logging
            implementations to be consumed by the core api."}
     clojure.tools.logging.impl
  (:refer-clojure :exclude [name]))

(defprotocol Logger
  "The protocol through which the core api will interact with an underlying logging
  implementation.  Implementations should at least support the six standard
  logging levels if they wish to work from the level-specific macros."
  (enabled? [logger level]
    "Check if a particular level is enabled for the given Logger.")
  (write! [logger level throwable message]
    "Writes a log message to the given Logger."))

(defprotocol LoggerFactory
  "The protocol through which the core api will obtain an instance satisfying Logger
  as well as providing information about the particular implementation being used.
  Implementations should be bound to *logger-factory* in order to be picked up by
  this library."
  (name [factory]
    "Returns some text identifying the underlying implementation.")
  (get-logger [factory logger-ns]
    "Returns an implementation-specific Logger by namespace."))

(defn common-logging-factory
  "Returns a Commons Logging-based implementation of the LoggerFactory protocol, or
  nil if not available."
  []
  (try
    (when (Type/GetType "Common.Logging.LogManager, Common.Logging")
      (eval
       `(do
          (extend Common.Logging.ILog
            Logger
            {:enabled?
             (fn [^Common.Logging.ILog logger# level#]
               (condp = level#
                 :trace (.IsTraceEnabled logger#)
                 :debug (.IsDebugEnabled logger#)
                 :info  (.IsInfoEnabled  logger#)
                 :warn  (.IsWarnEnabled  logger#)
                 :error (.IsErrorEnabled logger#)
                 :fatal (.IsFatalEnabled logger#)
                 (throw (ArgumentException. (str level#)))))
             :write!
             (fn [^Common.Logging.ILog logger# level# ^Exception e# msg#]
               (let [^String msg# (str msg#)]
                 (if e#
                   (condp = level#
                     :trace (.Trace logger# msg# e#)
                     :debug (.Debug logger# msg# e#)
                     :info  (.Info  logger# msg# e#)
                     :warn  (.Warn  logger# msg# e#)
                     :error (.Error logger# msg# e#)
                     :fatal (.Fatal logger# msg# e#)
                     (throw (ArgumentException. (str level#))))
                   (condp = level#
                     :trace (.Trace logger# msg#)
                     :debug (.Debug logger# msg#)
                     :info  (.Info  logger# msg#)
                     :warn  (.Warn  logger# msg#)
                     :error (.Error logger# msg#)
                     :fatal (.Fatal logger# msg#)
                     (throw (ArgumentException. (str level#)))))))})
          (reify LoggerFactory
            (name [_#]
              "Common.Logging")
            (get-logger [_# logger-ns#]
              (Common.Logging.LogManager/GetLogger ^String (str logger-ns#)))))))
    (catch Exception e nil)))

(defn find-factory
  "Returns the first non-nil value from common-logging-factory."
  []
  (or
   (common-logging-factory)
   (comment
     (throw 
      (Exception.
       "Valid logging implementation could not be found.")))))


