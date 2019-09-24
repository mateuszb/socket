(in-package :socket)

(defclass socket ()
  ((fd :type (signed-byte 32) :initarg :fd :initform -1 :reader socket-fd)))

(defclass listen-socket (socket)
  ((port :type (unsigned-byte 16)
	 :initarg :port
	 :initform 0
	 :reader socket-listen-port)))

(defcfun (make-socket "socket") :int
  (domain address-family)
  (type socket-type)
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

(defcfun (socket-shutdown "shutdown") :int
  (fd :int)
  (how direction))

(defcfun (fcntl-getfl "fcntl") :int
  (fd :int)
  (cmd :int))

(defcfun (fcntl-setfl "fcntl") :int
  (fd :int)
  (cmd :int)
  (arg :int))

(defcfun (getpeername "getpeername") :int
  (fd :int)
  (addr (:pointer (:struct sockaddr-in)))
  (addrlen (:pointer :socklen-t)))

(defcfun (gethostbyname "gethostbyname") (:pointer (:struct hostent))
  (hostname :string))

(defcfun (ioctl "ioctl") :int
  (fd :int)
  (request :ulong)
  (ptr :pointer))

(defcfun (setsockopt "setsockopt") :int
  (fd :int)
  (level socket-opt-level)
  (optname socket-opt)
  (optval :pointer)
  (optlen :socklen-t))

(defun get-rxbytes (fd)
  (with-foreign-object (rxbytes :int32)
    (let ((err (ioctl (socket-fd fd) +FIONREAD+ rxbytes)))
      (cond
	((< err 0) (error 'ioctl-failed))
	(t (cffi:mem-ref rxbytes :int32))))))

(defmethod print-object ((sock socket) stream)
  (format stream "#<socket: fd=~a>"
	  (socket-fd sock)))

(defmethod print-object ((sock listen-socket) stream)
  (format stream "#<listen-socket: fd=~a, port=~a>"
	  (socket-fd sock) (socket-listen-port sock)))

(define-condition socket-error ()
  ((fd :type (signed-byte 32) :initform -1 :initarg :fd)
   (msg :type 'string :initarg :msg :initform nil :reader socket-error-msg)))

(define-condition socket-bind-error (socket-error)
  ((port :type (unsigned-byte 16)
	 :initform 0 :initarg :port
	 :reader socket-bind-error-port))
  (:report
   (lambda (c s)
     (format s "bind(~a) error: ~a~%"
	     (socket-bind-error-port c)
	     (socket-error-msg c)))))

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

(define-condition operation-in-progress (error)
  ((fd :type (signed-byte 32) :initform -1 :initarg fd)))

(define-condition operation-would-block (error)
  ((fd :type (signed-byte 32) :initform -1 :initarg fd)))

(define-condition operation-interrupted (error)
  ())

(define-condition socket-eof (error)
  ((fd :type (signed-byte 32) :initform -1 :initarg :fd)))

(defun errno ()
  (sb-impl::get-errno))

(defun strerror (errcode)
  (sb-int:strerror errcode))

(defun bswap16 (x)
  (logior (ash x -8) (logand (ash x 8) #xFF00)))

(defun vec->uint (vec)
  (reduce (lambda (&optional x y)
	    (when (and x y)
	      (logior (ash x 8) y)))
	  vec))

(defun uint->vec (uint32)
  (loop for lowbit from 0 upto 24 by 8
     collect (ldb (byte 8 lowbit) uint32)))

(defun make-tcp-listen-socket
    (port &optional (bind-address #(0 0 0 0)) (backlog 10))
  (let ((desc (make-socket :AF-INET :SOCK-STREAM 0)))
    (when (= desc -1)
      (error 'socket-error :msg (errno)))

    (with-foreign-object (opt :int)
      (setf (mem-ref opt :int) 1)
      (let ((err (setsockopt desc :SOL-SOCKET
			     :SO-REUSEPORT opt
			     (foreign-type-size :int))))
	(when (= err -1)
	  (error 'socket-error :fd desc :msg (strerror (errno)))))
      (let ((err (setsockopt desc :SOL-SOCKET
			     :SO-REUSEADDR opt
			     (foreign-type-size :int))))
	(when (= err -1)
	  (error 'socket-error :fd desc :msg (strerror (errno))))))
	
    (with-foreign-object (addr '(:struct sockaddr-in))
      (with-foreign-slots ((sin-family sin-port (:pointer sin-addr)) addr (:struct sockaddr-in))
	(with-foreign-slots ((s-addr) sin-addr (:struct in-addr))
	  (setf sin-family (convert-to-foreign :AF-INET 'address-family)
		sin-port (bswap16 port)
		s-addr (vec->uint (reverse bind-address)))))

      (let ((err (bind desc addr (foreign-type-size '(:struct sockaddr-in)))))
	(when (= err -1)
	  (error 'socket-bind-error :port port :fd desc :msg (errno))))

      (let ((err (socket-listen desc backlog)))
	(when (= err -1)
	  (error 'socket-listen-error :port port :fd desc :msg (errno)))))

    (make-instance 'listen-socket :port port :fd desc)))

(defun make-tcp-socket (&optional non-blocking-p)
  (let ((desc (make-socket :AF-INET :SOCK-STREAM 0)))
    (when (= desc -1)
      (error 'socket-error :msg (errno)))
    (let ((socket (make-instance 'socket :fd desc)))
      (when non-blocking-p
	(set-non-blocking socket))
      socket)))

(defun send (socket buf len)
  (let ((err (socket-write (socket-fd socket) buf len)))
    (when (= err -1)
      (case *errno*
	(:EWOULDBLOCK
	 (error 'operation-would-block :fd (socket-fd socket)))
	(t (error 'socket-write-error :msg (errno)))))
    err))

(defun receive (socket bufaddrs lens)
  (with-foreign-object (iovs '(:struct iovec) (length lens))
    (labels ((fill-iov (iov addr len)
	       (with-foreign-slots ((iov-base iov-len) iov (:struct iovec))
		 (setf iov-base addr iov-len len))))
      (mapcar #'fill-iov
	      (loop for i from 0 below (length lens)
		 collect (mem-aptr iovs '(:struct iovec) i))
	      bufaddrs
	      lens))
    (let ((nread (socket-readv (socket-fd socket) iovs (length lens))))
      (cond
	((= nread -1)
	 (case *errno*
	   (:EINTR
	    (error 'operation-interrupted))
	   ((:EAGAIN :EWOULDBLOCK)
	    (error 'operation-would-block))
	   (t
	    (error 'socket-read-error :msg (errno) :fd (socket-fd socket)))))
	((= nread 0)
	 (error 'socket-eof :fd (socket-fd socket)))
	(t nread)))))

(defun connect (socket peer-addr port)
  (with-foreign-object (addr '(:struct sockaddr-in))
    (with-foreign-slots
	((sin-family sin-port (:pointer sin-addr)) addr (:struct sockaddr-in))
      (with-foreign-slots ((s-addr) sin-addr (:struct in-addr))
	(setf sin-family (convert-to-foreign :AF-INET 'address-family)
	      sin-port (bswap16 port)
	      s-addr (vec->uint (reverse peer-addr)))))

    (let* ((fd (socket-fd socket))
	   (size (foreign-type-size '(:struct sockaddr-in)))
	   (status (socket-connect fd addr size)))
      (when (= status -1)
	(let ((errcode *errno*))
	  (case errcode
	    (:EINPROGRESS
	     (error 'operation-in-progress :fd (socket-fd socket)))
	    (t
	     (format t "*errno* = ~a~%" errcode)
	     (error
	      'socket-connect-error
	      :peer peer-addr
	      :port port
	      :fd (socket-fd socket)
	      :msg errcode)))))))
  socket)

(defun disconnect (socket)
  (let ((err (socket-shutdown (socket-fd socket) :SHUT-RDWR)))
    (when (= err -1)
      (case *errno*
	(:ENOTCONN
	 (format t "warning: socket already disconnected~%"))
	(t (error
	    'socket-error :fd (socket-fd socket) :msg (errno))))))

  (sb-posix:close (socket-fd socket)))

(defun set-non-blocking (socket)
  (let ((fd (socket-fd socket)))
    (let ((flags (fcntl-getfl fd +F-GETFL+)))
      (let ((err (fcntl-setfl fd +F-SETFL+ (logior flags +O-NONBLOCK+))))
	(when (= err -1)
	  (error 'socket-error :msg (errno)))
	t))))

(defun set-blocking (socket)
  (let ((fd (socket-fd socket)))
    (let ((flags (fcntl-getfl fd +F-GETFL+)))
      (let ((err (fcntl-setfl fd +F-SETFL+ (logand flags (lognot +O-NONBLOCK+)))))
	(when (= err -1)
	  (error 'socket-error :msg (errno)))
	socket))))

(defun accept (socket)
  (with-foreign-objects ((addr '(:struct sockaddr-in)) (len :socklen-t))
    (setf (mem-ref len :socklen-t) (foreign-type-size '(:struct sockaddr-in)))
    (let ((newsock (socket-accept (socket-fd socket) addr len)))
      (when (= newsock -1)
	(let ((error-code *errno*))
	  (case error-code
	    (:EINTR (error 'operation-interrupted))
	    (:EAGAIN
	     (error 'operation-would-block :fd (socket-fd socket)))
	    (:EWOULDBLOCK
	     (error 'operation-would-block :fd (socket-fd socket)))
	    (t (error 'socket-error :msg error-code :fd (socket-fd socket))))))
      (make-instance 'socket :fd newsock))))

(defun get-peer-name (socket)
  (with-foreign-objects ((addr '(:struct sockaddr-in)) (len :socklen-t))
    (setf (mem-ref len :socklen-t) (foreign-type-size '(:struct sockaddr-in)))
    (loop for i from 0 below (foreign-type-size '(:struct sockaddr-in))
       do (setf (mem-aref addr :uint8 i) 0))
    (let ((err (getpeername socket addr len)))
      (unless (= err -1)
	(with-foreign-slots (((:pointer sin-addr) sin-port) addr (:struct sockaddr-in))
	  (with-foreign-slots ((s-addr) sin-addr (:struct in-addr))
	    (uint->vec s-addr)))))))

(defun get-host-addrs (hostname)
  (let ((addr (gethostbyname hostname)))
    (with-foreign-slots ((len addr-list) addr (:struct hostent))
      (loop for i from 0 below 10
	 for ent = (mem-aref addr-list :pointer i)
	 then (mem-aref addr-list :pointer i)
	 until (pointer-eq (null-pointer) ent)
	 collect
	   (uint->vec (foreign-slot-value ent '(:struct in-addr) 's-addr))))))

(defun get-host-addr (hostname)
  (car (get-host-addrs hostname)))

(defun set-reuse-addr (socket)
  (let ((fd (socket-fd socket)))
    (with-foreign-object (opt :int)
      (setf opt 1)
      (let ((err (setsockopt fd :SOL-SOCKET
			     :SO-REUSEADDR opt
			     (foreign-type-size :int))))
	(cond
	  ((= err -1) (error 'socket-error :fd fd :msg (strerror (errno))))
	  (t socket))))))

(defun set-reuse-port (socket)
  (let ((fd (socket-fd socket)))
    (with-foreign-object (opt :int)
      (setf opt 1)
      (let ((err (setsockopt fd :SOL-SOCKET
			     :SO-REUSEPORT opt
			     (foreign-type-size :int))))
	(cond
	  ((= err -1) (error 'socket-error :fd fd :msg (strerror (errno))))
	  (t socket))))))
