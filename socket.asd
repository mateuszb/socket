(defsystem "socket"
  :version "0.1.0"
  :author "Mateusz Berezecki"
  :license "BSD"
  :defsystem-depends-on ("cffi" "cffi-grovel")
  :serial t
  :components ((:file "packages")
	       (:cffi-grovel-file "grovel" :depends-on ("packages"))
	       #+freebsd
	       (:file "freebsd" :depends-on ("packages"))
	       (:file "socket" :depends-on ("packages" "grovel")))

  :depends-on ("cffi")
  :in-order-to ((test-op (test-op "socket/tests")))
  :description "A lightweight socket library that is easy to integrate with epoll/kqueue mechanisms.")

(defsystem "socket/tests"
  :depends-on ("prove")
  :defsystem-depends-on (:prove-asdf)
  :serial t
  :components ((:module "tests" :components ((:test-file "test"))))
  :perform (test-op :after (o c)
		    (funcall (intern #. (string :run) :prove) c)))
