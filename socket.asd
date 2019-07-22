(defsystem "socket"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :defsystem-depends-on ("cffi" "cffi-grovel")
  :serial t
  :components ((:file "packages")
	       (:cffi-grovel-file "grovel" :depends-on ("packages"))
	       (:file "socket" :depends-on ("packages" "grovel")))
  :depends-on ("cffi")
  :description "A lightweight socket library that is easy to integrate with epoll/kqueue mechanisms.")

