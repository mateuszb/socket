(defpackage socket/tests
  (:use :cl :socket :prove :sb-alien :sb-sys))

(in-package :socket/tests)

(defvar srv nil)
(defvar srv-cli nil)
(defvar cli nil)

(defparameter +test-port+ 1261)

(defun blocking-io-test ()
  (let ((srvbuf (make-alien (unsigned 8) 16)))

    (setf srv (make-tcp-listen-socket +test-port+))
    (isnt (socket-fd srv) -1)

    (setf cli (make-tcp-socket))
    (isnt (socket-fd cli) -1)

    (is (connect cli #(127 0 0 1) +test-port+) cli)

    (setf srv-cli (accept srv))
    (isnt (socket-fd srv-cli) -1)

    (let ((alienstr (make-alien-string "test" :null-terminate nil)))
      (send cli (alien-sap alienstr) 4))
    (receive srv-cli (list (alien-sap srvbuf)) '(4))

    (is (map 'string (lambda (x)
		       (code-char (deref srvbuf x)))
	     '(0 1 2 3)) "test")

    (disconnect srv)
    (disconnect cli)
    (disconnect srv-cli)))

(defun nonblocking-io-test ()
  (let ((srvbuf (make-alien (unsigned 8) 16)))

    (setf srv (make-tcp-listen-socket +test-port+))
    (isnt (socket-fd srv) -1)
    (set-non-blocking srv)

    (setf cli (make-tcp-socket))
    (isnt (socket-fd cli) -1)
    (set-non-blocking cli)

    (is (handler-case (connect cli #(127 0 0 1) +test-port+)
	  (operation-in-progress () t)) t)

    (sleep 0.1)
    (setf srv-cli (accept srv))
    (isnt (socket-fd srv-cli) -1)
    (set-non-blocking srv-cli)

    (let ((alienstr (make-alien-string "test" :null-terminate nil)))
      (send cli (alien-sap alienstr) 4))
    (receive srv-cli (list (alien-sap srvbuf)) '(4))

    (is (map 'string (lambda (x)
		       (code-char (deref srvbuf x)))
	     '(0 1 2 3)) "test")

    (disconnect srv)
    (disconnect cli)
    (disconnect srv-cli)))

(defun wraparound-io-test ()
  (let ((srvbuf (make-alien (unsigned 8) 16))
	(clibuf (make-alien (unsigned 8) 16)))

    (setf srv (make-tcp-listen-socket +test-port+))
    (isnt (socket-fd srv) -1)
    (set-non-blocking srv)

    (setf cli (make-tcp-socket))
    (isnt (socket-fd cli) -1)
    (set-non-blocking cli)

    (is (handler-case (connect cli #(127 0 0 1) +test-port+)
	  (operation-in-progress () t)) t)

    (sleep 0.1)
    (setf srv-cli (accept srv))
    (isnt (socket-fd srv-cli) -1)
    (set-non-blocking srv-cli)

    (let ((alienstr (make-alien-string "test" :null-terminate nil)))
      (send cli (alien-sap alienstr) 4))

    (is (receive srv-cli (list (alien-sap srvbuf)) '(16)) 4)

    (is (map 'string (lambda (x)
		       (code-char (deref srvbuf x)))
	     '(0 1 2 3)) "test")

    (loop for i from 0 below 16 do (setf (deref clibuf i) (char-code #\-)))
    (loop for i from 0 below 16 do (setf (deref srvbuf i) (+ i (char-code #\A))))

    ;; before send
    (format t "server buf:~%")
    (loop for i from 0 below 16 do (format t "~a" (code-char (deref srvbuf i))))
    (terpri)

    (format t "client buf:~%")
    (loop for i from 0 below 16 do (format t "~a" (code-char (deref clibuf i))))
    (terpri)

    (is (send srv-cli (alien-sap srvbuf) 12) 12)
    (is (receive cli (list (alien-sap clibuf)) '(12)) 12)

    ;; after send
    (format t "server buf:~%")
    (loop for i from 0 below 16 do (format t "~a" (code-char (deref srvbuf i))))
    (terpri)

    (format t "client buf:~%")
    (loop for i from 0 below 16 do (format t "~a" (code-char (deref clibuf i))))
    (terpri)

    (loop for i from 0 below 16 do (setf (deref clibuf i) (char-code #\-)))
    (is (send srv-cli (alien-sap srvbuf) 12) 12)
    (is (receive cli (list (sap+ (alien-sap clibuf) 12) (alien-sap clibuf)) '(4 8)) 12)

    (loop for i from 0 below 16 do (format t "~a" (code-char (deref clibuf i))))

    (disconnect srv)
    (disconnect cli)
    (disconnect srv-cli)))

(plan 10)
(blocking-io-test)
(nonblocking-io-test)
(wraparound-io-test)
(finalize)
