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
   :socket-error
   :socket-read-error
   :socket-write-error))
