(ns clojure.clr.socketrepl
  (:require [clojure.main :as main]
            [clojure.tools.logging :as log])
  (:import [System.Threading Thread ThreadStart]
           [System.Net Dns]
           [System.Net.Sockets TcpListener]
           [System.Net.Security SslStream RemoteCertificateValidationCallback LocalCertificateSelectionCallback]
           [System.Security.Authentication SslProtocols]
           [System.Security.Cryptography.X509Certificates X509Certificate X509Certificate2]))

(defn- provide-local-cert [o target-host local-certs remote-cert acceptable-issuers ssl-cert]
  ssl-cert)

(defn- validate-server-cert [o cert chain policy-errors]
  true)

(defn- client-ssl-stream [stream ssl-server-name ssl-cert]
  (let [ssl-stream (SslStream. stream
                               false
                               (gen-delegate RemoteCertificateValidationCallback [o ce ch pe] (validate-server-cert o ce ch pe))
                               (gen-delegate LocalCertificateSelectionCallback [o t l r a] (provide-local-cert o t l r a ssl-cert)))]
    (.AuthenticateAsClient ssl-stream ssl-server-name)
    ssl-stream))

(defn- decode-target [target]
  (re-matches #"(([^@]+)@)?(([^:]+):)?(\d+)" target))

(defn start-repl-client
  [target & {:keys [ssl-cert]}]
  (if-let [[_ _ ssl-server-name _ host port] (decode-target target)]
    (let [host (if (empty? host) "localhost" host)]
      (println host port)
      (with-open [client (System.Net.Sockets.TcpClient. host port)
                  stream (.GetStream client)
                  ;;stream (if ssl-server-name (client-ssl-stream stream ssl-server-name ssl-cert) stream)
                  writer (System.IO.StreamWriter. stream)
                  reader (System.IO.StreamReader. stream)]
        (let [request-prompt (Object.)
              request-exit (Object.)
              send-eval
              (fn [input]
                (.WriteLine writer (pr-str input)))
              send-newline (fn [] (.WriteLine writer ""))]
          (set! (.AutoFlush writer) true)
          (println "Connected to" target)
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
                (recur)))))))
    (println "Invalid repl target" target)))

(defn- server-ssl-stream [stream ssl-cert validate-client-cert]
  (let [ssl-stream (SslStream. stream false (when validate-client-cert (gen-delegate RemoteCertificateValidationCallback [o ce ch pe] (validate-client-cert o ce ch pe))))]
    (.AuthenticateAsServer ssl-stream ssl-cert (when validate-client-cert) SslProtocols/Tls true)
    ssl-stream))

(defn- accept-client [server ssl-cert validate-client-cert]
  (try
    (let [client (.AcceptTcpClient server)
          stream (.GetStream client)
          ;;stream (if ssl-cert (server-ssl-stream stream ssl-cert validate-client-cert) stream)
          reader (System.IO.StreamReader. stream)
          writer (System.IO.StreamWriter. stream)]
      (set! (.AutoFlush writer) true)
      (try
        (binding [*out* writer
                  *err* writer
                  *ns* (find-ns 'user)]
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
                 (log/warn ex "Repl read error"))))))
        (catch Exception ex
          (log/warn ex "Repl error"))
        (finally (do
                   (.Dispose reader)
                   (.Dispose writer)
                   (.Close client)))))
    (catch Exception ex
      (log/warn ex "Client accept error"))))

(defn- repl-server-proc [port running ssl-cert validate-client-cert]
  (let [server (TcpListener. (first (.AddressList (Dns/GetHostEntry "localhost"))) port)]
    (try
      (.Start server)
      (while @running
        (log/info "Listening for REPL connections on port" port)
        (accept-client server ssl-cert validate-client-cert))
      (finally
        (.Stop server)
        (log/info "REPL server closed")))))

(defn start-repl-server
  [port & {:keys [ssl-cert validate-client-cert]}]
  (let [running (atom true)
        thread
        (doto (Thread. (gen-delegate ThreadStart [] (repl-server-proc port running ssl-cert validate-client-cert)))
          (.set_IsBackground true)
          (.Start))]
    {:thread thread
     :running running}))
