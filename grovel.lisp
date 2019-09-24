(progn
  (in-package :socket)

  (include "sys/types.h"
	   "sys/socket.h"
	   "sys/ioctl.h"
	   "netinet/in.h"
	   "netdb.h"
	   "errno.h"
	   "string.h"
	   "unistd.h"
	   "fcntl.h")

  (constantenum
   (address-family :base-type :int :define-constants t)
   ((:af-inet "AF_INET"))
   ((:af-inet6 "AF_INET6")))

  (constantenum
   (socket-type :base-type :int :define-constants t)
   ((:sock-stream "SOCK_STREAM")))

  (constantenum
   (socket-opt-level :base-type :int :define-constants t)
   ((:sol-socket "SOL_SOCKET")))

  (constantenum
   (socket-opt :base-type :int :define-constants t)
   ((:SO-REUSEADDR "SO_REUSEADDR"))
   ((:SO-REUSEPORT "SO_REUSEPORT"))
   ((:SO-KEEPALIVE "SO_KEEPALIVE"))
   ((:SO-LINGER "SO_LINGER"))
   ((:SO-SNDBUF "SO_SNDBUF"))
   ((:SO-RCVBUF "SO_RCVBUF"))
   ((:SO-RCVTIMEO "SO_RCVTIMEO"))
   ((:SO-SNDTIMEO "SO_SNDTIMEO")))

  (constant (+F-SETFL+ "F_SETFL"))
  (constant (+F-GETFL+ "F_GETFL"))

  (constant (+O-NONBLOCK+ "O_NONBLOCK"))

  (constantenum
   (error-code :base-type :int :define-constants t)
   ((:EAGAIN "EAGAIN"))
   ((:EWOULDBLOCK "EWOULDBLOCK"))
   ((:EINPROGRESS "EINPROGRESS"))
   ((:EINTR "EINTR"))
   ((:EIO "EIO"))
   ((:ENOTCONN "ENOTCONN"))
   ((:ECONNRESET "ECONNRESET"))
   ((:EBADF "EBADF"))
   ((:EACCESS "EACCESS"))
   ((:ENOTSOCK "ENOTSOCK"))
   ((:EFAULT "EFAULT"))
   ((:EMSGSIZE "EMSGSIZE"))
   ((:ENOBUFS "ENOBUFS"))
   ((:EHOSTUNREACH "EHOSTUNREACH"))
   ((:EISCONN "EISCONN"))
   ((:ECONNREFUSED "ECONNREFUSED"))
   ((:EHOSTDOWN "EHOSTDOWN"))
   ((:ENETDOWN "ENETDOWN"))
   ((:EADDRNOTAVAIL "EADDRNOTAVAIL"))
   ((:EPIPE "EPIPE"))
   )

  (constant (+FIONREAD+ "FIONREAD"))

  (constantenum
   (direction :base-type :int :define-constants t)
   ((:SHUT-RD "SHUT_RD"))
   ((:SHUT-WR "SHUT_WR"))
   ((:SHUT-RDWR "SHUT_RDWR")))

  (ctype :size-t "size_t")
  (ctype :ssize-t "ssize_t")
  (ctype :socklen-t "socklen_t")
  (ctype :in-port-t "in_port_t")

  (ctype :in-addr-t "in_addr_t")
  (cstruct in-addr "struct in_addr"
	   (s-addr "s_addr" :type :in-addr-t))

  (ctype :sa-family-t "sa_family_t")
  (cstruct sockaddr-in "struct sockaddr_in"
	   (sin-family "sin_family" :type :sa-family-t)
	   (sin-addr "sin_addr" :type (:struct in-addr))
	   (sin-port "sin_port" :type :in-port-t))

  (cstruct iovec "struct iovec"
	   (iov-base "iov_base" :type :pointer)
	   (iov-len "iov_len" :type :size-t))

  (cstruct hostent "struct hostent"
	   (name "h_name" :type :pointer)
	   (aliases "h_aliases" :type :pointer)
	   (addr-type "h_addrtype" :type :int)
	   (len "h_length" :type :int)
	   (addr-list "h_addr_list" :type :pointer))

  )

