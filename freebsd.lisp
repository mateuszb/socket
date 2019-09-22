(in-package :socket)

;; we declare this dynamically because the groveller is broken in
;; freebsd 12
(defcvar ("errno" *errno*) :int)
(defcvar ("errno" raw-errno) :int)
