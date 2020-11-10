
(in-package #:prolog-engine-client)

#|
Default engine paths as global variables. Change these to your local paths!
|#

(defparameter *default-scryer-prolog-path*
  #p"/home/mark/Projects/Rust/scryer-prolog/target/release/scryer-prolog")

(defparameter *default-xsb-prolog-path*
  #p"/home/mark/XSB/bin/xsb")

(defstruct prolog-engine-client
  "A structure containing the name, path and socket of a Prolog
server, confusingly termed a client, which actually hosts a
server. The server compiles and loads the translated Prolog KB sent to
it by cl-psoatransrun. It receives query strings it evaluates against
the translated KB, and sends back answer strings."
  (host :scryer :type (or (eql :xsb) (eql :scryer))) ;; The name of the host system.
  (path *default-scryer-prolog-path* :type pathname)
  socket ;; Either a single bidirectional socket (Scryer) or two unidirectional sockets (XSB).
  )

(defstruct xsb-engine-socket
  "Defines a \"socket\" for use with XSB sockets API, which supports only uni-directional
sockets."
  read-socket
  write-socket)

(defun make-engine-client (system)
  "Configure a prolog-engine-client according to the path and socket
value and type defaults. Sockets are initialized separately."
  (ecase system
    (:scryer (make-prolog-engine-client
              :host :scryer
              :path *default-scryer-prolog-path*))
    (:xsb (make-prolog-engine-client
           :host :xsb
           :path *default-xsb-prolog-path*
           :socket (make-xsb-engine-socket)))))

(defun open-socket-to-prolog (client &key port)
  "Connect a socket to the type, based on the host type. Scryer uses
bidirectional sockets while XSB needs two unidirectional sockets, of
which the write socket must connect first."
  (check-type port (integer 0 65536))
  (setf (prolog-engine-client-socket client)
        (ecase (prolog-engine-client-host client)
          (:scryer (socket-connect "127.0.0.1" port))
          (:xsb (make-xsb-engine-socket
                 :write-socket (socket-connect "127.0.0.1" port)
                 :read-socket (socket-connect "127.0.0.1" port)))))
  (prolog-engine-client-socket client))

#|

The generic functions out-socket-stream, in-socket-stream and
close-socket are used in the #:psoatransrun and #:psoatransrun-tests
packages to access and close socket streams. Their arguments are
specialized to the socket types used by the two variants of
prolog-engine-client, allowing functions in those packages to ignore
client implementation details.

|#

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
