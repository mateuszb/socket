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

  (cvar ("errno" *errno*) :int)
  (constant (+af-inet+ "AF_INET"))
  (constant (+af-inet6+ "AF_INET6"))
  (constant (+sock-stream+ "SOCK_STREAM"))

  (constant (+SOL-SOCKET+ "SOL_SOCKET"))
  (constant (+SO-REUSEADDR+ "SO_REUSEADDR"))

  (constant (+F-SETFL+ "F_SETFL"))
  (constant (+F-GETFL+ "F_GETFL"))

  (constant (+O-NONBLOCK+ "O_NONBLOCK"))

  (constant (+EAGAIN+ "EAGAIN"))
  (constant (+EWOULDBLOCK+ "EWOULDBLOCK"))
  (constant (+EINPROGRESS+ "EINPROGRESS"))
  (constant (+EINTR+ "EINTR"))
  (constant (+EIO+ "EIO"))
  (constant (+ENOTCONN+ "ENOTCONN"))

  (constant (+FIONREAD+ "FIONREAD"))

  (constant (+SHUT-RDWR+ "SHUT_RDWR"))
  (constant (+SHUT-RD+ "SHUT_RD"))
  (constant (+SHUT-WR+ "SHUT_WR"))

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
	   (addr-list "h_addr_list" :type :pointer)))
