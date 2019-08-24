(defpackage socket
  (:use :cl :cffi)
  (:export
   :make-tcp-socket
   :make-tcp-listen-socket
   :send
   :receive
   :connect
   :disconnect
   :set-non-blocking
   :set-blocking
   :accept
   :socket-fd
   :operation-in-progress
   :operation-would-block
   :operation-interrupted
   :socket-error
   :socket-read-error
   :socket-write-error
   :socket-eof
   :get-rxbytes
   :get-peer-name
   :get-host-addr
   :get-host-addrs))
