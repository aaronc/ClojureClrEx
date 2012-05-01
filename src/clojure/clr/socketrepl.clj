(ns clojure.clr.socketrepl
  (:require [clojure.main :as main])
  (:import [System.Threading Thread ThreadStart]
           [System.Net.Sockets TcpListener]))

(defn start-repl-client
  [port]
  (with-open [client (System.Net.Sockets.TcpClient. "127.0.0.1" port)
              stream (.GetStream client)
              writer (System.IO.StreamWriter. stream)
              reader (System.IO.StreamReader. stream)]
    (let [request-prompt (Object.)
          request-exit (Object.)
          send-eval
          (fn [input]
             (.WriteLine writer (pr-str input)))
          send-newline (fn [] (.WriteLine writer ""))]
      (set! (.AutoFlush writer) true)
      (.Start (Thread. (gen-delegate ThreadStart []
                                     (let [buf (make-array Char 1)]
                                       (while (> (.Read reader buf 0 1) 0)
                                         (Console/Write (String. buf)))))))
      (loop []
        (let [input (main/repl-read request-prompt request-exit)]
          (when-not (= input request-exit)
            (if (= input request-prompt)
              (send-newline)
              (send-eval input))
            (comment (when-let [line (.ReadLine reader)]
                       (println line)))
            (recur)))))))

(defn start-repl-server
  [port]
  (let [server (TcpListener. port)]
    (try (.Start server) 
         (let [client (.AcceptTcpClient server)
               stream (.GetStream client)
               reader (System.IO.StreamReader. stream)
               writer (System.IO.StreamWriter. stream)]
           (set! (.AutoFlush writer) true)
           (try
             (binding [*out* writer
                       *err* writer]
               (main/repl
                :read
                (fn [request-prompt request-exit]
                  (try
                    (let [input (.ReadLine reader)]
                      (cond
                       (nil? input) request-exit
                       (empty? input) request-prompt
                       :default (read-string input)))
                    (catch Exception ex
                      (println ex))))))
             (catch Exception ex
               (println ex))
             (finally (do
                        (.Dispose reader)
                        (.Dispose writer)
                        (.Close client)))))
         (finally (.Stop server)))))