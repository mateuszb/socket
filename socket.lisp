(in-package :socket)

(defclass socket (t)
  ((fd :type (signed-byte 32) :initarg :fd :initform -1 :reader socket-fd)))

(defclass listen-socket (socket)
  ((port :type (unsigned-byte 16) :initarg :port :initform 0 :reader socket-listen-port)))

(defcfun socket :int
  (domain :int)
  (type :int)
  (proto :int))

(defcfun bind :int
  (fd :int)
  (addr (:pointer (:struct sockaddr-in)))
  (addrlen :socklen-t))

(defcfun (socket-listen "listen") :int
  (fd :int)
  (backlog :int))

(defcfun (socket-accept "accept") :int
  (fd :int)
  (addr (:pointer (:struct sockaddr-in)))
  (addrlen (:pointer :socklen-t)))

(defcfun (socket-connect "connect") :int
  (fd :int)
  (addr (:pointer (:struct sockaddr-in)))
  (addrlen :socklen-t))

(defcfun (socket-write "write") :ssize-t
  (fd :int)
  (buf :pointer)
  (count :size-t))

(defcfun (socket-read "read") :ssize-t
  (fd :int)
  (buf :pointer)
  (count :size-t))

(defcfun (socket-readv "readv") :ssize-t
  (fd :int)
  (iov (:pointer (:struct iovec)))
  (iovcnt :int))

(defcfun (socket-writev "writev") :ssize-t
  (fd :int)
  (iov (:pointer (:struct iovec)))
  (iovcnt :int))

(defcfun (socket-close "close") :int
  (fd :int))

(defcfun (fcntl-getfl "fcntl") :int
  (fd :int)
  (cmd :int))

(defcfun (fcntl-setfl "fcntl") :int
  (fd :int)
  (cmd :int)
  (arg :int))

(defmethod print-object ((sock socket) stream)
  (format stream "#<socket: fd=~a>" (socket-fd sock)))

(defmethod print-object ((sock listen-socket) stream)
  (format stream "#<listen-socket: fd=~a, port=~a>" (socket-fd sock) (socket-listen-port sock)))

(define-condition socket-error ()
  ((fd :type (signed-byte 32) :initform -1 :initarg :fd)
   (msg :type 'string :initarg :msg :initform nil :reader socket-error-msg)))

(define-condition socket-bind-error (socket-error)
  ((port :type (unsigned-byte 16) :initform 0 :initarg :port :reader socket-bind-error-port))
  (:report (lambda (c s)
	     (format s "bind(~a) error: ~a~%" (socket-bind-error-port c) (socket-error-msg c)))))

(define-condition socket-listen-error (socket-error)
  ((port :type (unsigned-byte 16) :initform 0 :initarg :port)))

(define-condition socket-write-error (socket-error)
  ()
  (:report (lambda (c s)
	     (format s "socket-write-error error: ~a~%" (socket-error-msg c)))))

(define-condition socket-read-error (socket-error)
  ()
  (:report (lambda (c s)
	     (format s "socket-read-error error: ~a~%" (socket-error-msg c)))))

(define-condition socket-connect-error (socket-error)
  ((peer :initarg :peer :initform nil :reader socket-connect-error-peer))
  (:report (lambda (c s)
	     (format s "connect() error when connecting to ~a: ~a~%"
		     (socket-connect-error-peer c)
		     (socket-error-msg c)))))

(define-condition operation-in-progress ()
  ((fd :type (signed-byte 32) :initform -1 :initarg fd)))

(define-condition operation-would-block ()
  ((fd :type (signed-byte 32) :initform -1 :initarg fd)))

(defcfun strerror :string
  (errno :int))

(defun errno ()
  (strerror *errno*))

(defun bswap16 (x)
  (logior (ash x -8) (logand (ash x 8) #xFF00)))

(defun vec->uint (vec)
  (reduce (lambda (&optional x y)
	    (when (and x y)
	      (logior (ash x 8) y)))
	  vec))

(defun make-tcp-listen-socket (port &optional (bind-address #(0 0 0 0)) (backlog 10))
  (let ((desc (socket +AF-INET+ +SOCK-STREAM+ 0)))
    (when (= desc -1)
      (error (make-condition 'socket-error :msg (errno))))

    (with-foreign-object (addr '(:struct sockaddr-in))
      (with-foreign-slots ((sin-family sin-port (:pointer sin-addr)) addr (:struct sockaddr-in))
	(with-foreign-slots ((s-addr) sin-addr (:struct in-addr))
	  (setf sin-family +AF-INET+
		sin-port (bswap16 port)
		s-addr (vec->uint (reverse bind-address)))))

      (let ((err (bind desc addr (foreign-type-size '(:struct sockaddr-in)))))
	(when (= err -1)
	  (error (make-condition 'socket-bind-error :port port :fd desc :msg (errno)))))

      (let ((err (socket-listen desc backlog)))
	(when (= err -1)
	  (error (make-condition 'socket-listen-error :port port :fd desc :msg (errno))))))

    (make-instance 'listen-socket :port port :fd desc)))

(defun make-tcp-socket ()
  (let ((desc (socket +AF-INET+ +SOCK-STREAM+ 0)))
    (when (= desc -1)
      (error (make-condition 'socket-error :msg (errno))))
    (make-instance 'socket :fd desc)))

(defun send (socket buf)
  (with-foreign-object (fbuf :uint8 (length buf))
    (loop
       for i from 0 below (length buf)
       for c across buf do (setf (mem-aref fbuf :uint8 i) (char-code (aref buf i))))
    (let ((err (socket-write (socket-fd socket) fbuf (length buf))))
      (when (= err -1)
	(cond
	  ((= *errno* +EWOULDBLOCK+)
	   (error (make-condition 'operation-would-block :fd (socket-fd socket))))
	  (t
	   (error (make-condition 'socket-write-error :msg (errno))))))
      err)))

(defun receive (socket buf)
  (with-foreign-object (fbuf :uint8 (length buf))
    (let ((err (socket-read (socket-fd socket) fbuf (length buf))))
      (when (= err -1)
	(cond
	  ((= *errno* +EWOULDBLOCK+)
	   (error (make-condition 'operation-would-block :fd (socket-fd socket))))
	  (t
	   (error (make-condition 'socket-read-error :msg (errno))))))
      (loop for i from 0 below err
	 do (setf (aref buf i) (mem-aref fbuf :uint8 i)))
      err)))

(defun connect (socket peer-addr port)
  (with-foreign-object (addr '(:struct sockaddr-in))
    (with-foreign-slots ((sin-family sin-port (:pointer sin-addr)) addr (:struct sockaddr-in))
      (with-foreign-slots ((s-addr) sin-addr (:struct in-addr))
	(setf sin-family +AF-INET+
	      sin-port (bswap16 port)
	      s-addr (vec->uint (reverse peer-addr)))))

    (let ((err (socket-connect (socket-fd socket) addr (foreign-type-size '(:struct sockaddr-in)))))
      (when (= err -1)
	(cond
	  ((= *errno* +EINPROGRESS+)
	   (error (make-condition 'socket::operation-in-progress :fd (socket-fd socket))))
	  (t
	   (error
	    (make-condition 'socket-connect-error
			    :peer peer-addr
			    :port port
			    :fd (socket-fd socket)
			    :msg (errno))))))))
  socket)

(defun disconnect (socket)
  (let ((err (socket-close (socket-fd socket))))
    (when (= err -1)
      (error (make-condition 'socket-error :fd (socket-fd socket) :msg (errno))))))

(defun set-non-blocking (socket)
  (let ((fd (socket-fd socket)))
    (let ((flags (fcntl-getfl fd +F-GETFL+)))
      (let ((err (fcntl-setfl fd +F-SETFL+ (logior flags +O-NONBLOCK+))))
	(when (= err -1)
	  (error (make-condition 'socket-error :msg (errno))))
	t))))

(defun set-blocking (socket)
  (let ((fd (socket-fd socket)))
    (let ((flags (fcntl-getfl fd +F-GETFL+)))
      (let ((err (fcntl-setfl fd +F-SETFL+ (logand flags (lognot +O-NONBLOCK+)))))
	(when (= err -1)
	  (error (make-condition 'socket-error :msg (errno))))
	socket))))

(defun accept (socket)
  (with-foreign-objects ((addr '(:struct sockaddr-in))
			 (len :socklen-t))
    (setf (mem-ref len :socklen-t) (foreign-type-size '(:struct sockaddr-in)))
    (let ((err (socket-accept (socket-fd socket) addr len)))
      (when (= err -1)
	(error (make-condition 'socket-error :msg (errno))))
      (make-instance 'socket :fd err))))
