
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
  (path #p"" :type pathname)
  (pending-query nil :type (or null string)) ;; A pending query, i.e., one that prompted a KB recompilation.
  process ;; An object representing the Prolog subprocess.
  )

(defun make-engine-client (system)
  "Configure a prolog-engine-client according to the path and socket
value and type defaults. Sockets are initialized separately."
  (ecase system
    (:scryer (make-prolog-engine-client
              :host :scryer
              :path *default-scryer-prolog-path*))
    (:xsb (make-prolog-engine-client
           :host :xsb
           :path *default-xsb-prolog-path*))))

(defun enqueue-query (engine-client query-string)
  (setf (prolog-engine-client-pending-query engine-client)
        query-string))

(defun dequeue-query (engine-client)
  (prog1 (prolog-engine-client-pending-query engine-client)
    (enqueue-query engine-client nil)))

(defun process-input-stream (engine-client)
  (sb-ext:process-input (prolog-engine-client-process engine-client)))

(defun process-output-stream (engine-client)
  (sb-ext:process-output (prolog-engine-client-process engine-client)))

(defun terminate-engine-client (engine-client)
  (write-line "halt." (process-input-stream engine-client))
  (finish-output (process-input-stream engine-client))

  (sb-ext:process-wait (prolog-engine-client-process engine-client))
  (sb-ext:process-close (prolog-engine-client-process engine-client))

  (close (process-input-stream engine-client) :abort t)
  (close (process-output-stream engine-client) :abort t))
