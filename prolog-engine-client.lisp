
(in-package #:prolog-engine-client)

(defparameter *default-scryer-prolog-path*
  "/home/mark/Projects/Rust/scryer-prolog/target/release/scryer-prolog")

(defparameter *default-xsb-prolog-path*
  "/home/mark/XSB/bin/xsb")

(defstruct prolog-engine-client
  host
  path
  socket)

(defstruct xsb-engine-socket
  read-socket
  write-socket)

(defun make-engine-client (system)
  (ecase system
    (:scryer (make-prolog-engine-client
              :host :scryer
              :path *default-scryer-prolog-path*))
    (:xsb (make-prolog-engine-client
           :host :xsb
           :path *default-xsb-prolog-path*
           :socket (make-xsb-engine-socket)))))

(defun connect-to-prolog (client &key port)
  (check-type port (integer 0 65536))
  (setf (prolog-engine-client-socket client)
        (ecase (prolog-engine-client-host client)
          (:scryer (socket-connect "127.0.0.1" port))
          (:xsb (make-xsb-engine-socket
                 :write-socket (socket-connect "127.0.0.1" port)
                 :read-socket (socket-connect "127.0.0.1" port)))))
  (prolog-engine-client-socket client))

(defgeneric out-socket-stream (socket))

(defgeneric in-socket-stream (socket))

(defmethod out-socket-stream ((socket usocket))
  (socket-stream socket))

(defmethod in-socket-stream ((socket usocket))
  (socket-stream socket))

(defmethod out-socket-stream ((socket xsb-engine-socket))
  (socket-stream (xsb-engine-socket-write-socket socket)))

(defmethod in-socket-stream ((socket xsb-engine-socket))
  (socket-stream (xsb-engine-socket-read-socket socket)))

(defgeneric close-socket (socket))

(defmethod close-socket ((socket usocket))
  (socket-close socket))

(defmethod close-socket ((socket xsb-engine-socket))
  (socket-close (xsb-engine-socket-write-socket socket))
  (socket-close (xsb-engine-socket-read-socket socket)))
