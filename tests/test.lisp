(defpackage socket/tests
  (:use :cl :socket :prove))

(in-package :socket/tests)

(defvar srv nil)
(defvar srv-cli nil)
(defvar cli nil)

(defparameter +test-port+ 1236)

(defun blocking-io-test ()

  (setf srv (make-tcp-listen-socket +test-port+))
  (is (/= (socket-fd srv) -1) t)

  (setf cli (make-tcp-socket))
  (is (/= (socket-fd cli) -1) t)

  (is (connect cli #(127 0 0 1) +test-port+) cli)

  (setf srv-cli (accept srv))
  (isnt (socket-fd srv-cli) -1)

  (let ((result (make-array 4)))
    (send cli "test")
    (receive srv-cli result)
    (is (map 'string #'code-char result) "test"))

  (disconnect srv)
  (disconnect cli)
  (disconnect srv-cli))

(defun nonblocking-io-test ()
  (setf srv (make-tcp-listen-socket +test-port+))
  (is (/= (socket-fd srv) -1) t)
  (set-non-blocking srv)

  (setf cli (make-tcp-socket))
  (is (/= (socket-fd cli) -1) t)
  (set-non-blocking cli)

  (is (handler-case (connect cli #(127 0 0 1) +test-port+)
	(operation-in-progress () t)) t)

  (sleep 0.1)

  (setf srv-cli (accept srv))
  (isnt (socket-fd srv-cli) -1)
  (set-non-blocking srv-cli)

  (let ((result (make-array 4)))
    (handler-case (receive srv-cli result)
      (operation-would-block (c)
	(is (type-of c) 'operation-would-block))))

  (let ((result (make-array 4)))
    (send cli "test")
    (receive srv-cli result)
    (is (map 'string #'code-char result) "test"))

  (disconnect srv)
  (disconnect cli)
  (disconnect srv-cli))

(plan 10)
(blocking-io-test)
(nonblocking-io-test)
(finalize)
