
(in-package #:prolog-engine-client)

#|
#:prolog-engine-client exports an abstract interface over the
implementation details of how the underlying Prolog engine is managed
and interacted with as a subprocess of SBCL.

Details vary between systems -- the XSB Prolog server (xsb_server.pl)
requires just the subprocess instance while the Scryer Prolog
server (scryer_server.pl) requires the subprocess instance and a
usocket object.
|#

#|
Default engine paths as global variables. Change these to your local paths!
|#

(defparameter *default-scryer-prolog-path*
  #p"/home/mark/Projects/Rust/scryer-prolog/target/release/scryer-prolog")

(defparameter *default-xsb-prolog-path*
  #p"/home/mark/XSB/bin/xsb"
 ;;#p"c:/program files (x86)/xsb/config/x64-pc-windows/bin/xsb.exe"
  )

(defstruct prolog-engine-client
  "A structure comprised by the name, path and (sub)process of a Prolog
server. The server compiles and loads the translated Prolog KB sent to
it by cl-psoatransrun. It receives query strings it evaluates against
the translated KB, and sends back answer bindings as strings."
  (host :scryer :type (or (eql :xsb) (eql :scryer))) ;; The name of the host system.
  (path #p"" :type pathname)
  process ;; An object representing the Prolog subprocess.
  input-stream
  output-stream)

(defstruct scryer-client
  "Contains the means of communicating with a Scryer Prolog server -- its subprocess
and (bidirectional) socket objects."
  process
  socket)

(defun make-engine-client (system)
  "Configure a prolog-engine-client according to the path
default. Processes are initialized separately."
  (ecase system
    (:scryer (make-prolog-engine-client
              :host :scryer
              :path *default-scryer-prolog-path*))
    (:xsb (make-prolog-engine-client
           :host :xsb
           :path *default-xsb-prolog-path*))))

(defun close-prolog-engine-client (engine-client)
  "Shutdown \"engine-client\" by closing the attached streams and socket,
if one exists."
  (with-slots (host process input-stream output-stream)
      engine-client

    (close input-stream  :abort t)
    (close output-stream :abort t)

    (setf input-stream  nil
          output-stream nil)

    (ecase host
      (:xsb
       (sb-ext:process-wait process)
       (sb-ext:process-close process))
      (:scryer
       (with-slots (process socket)
           process

         (socket-close socket)
         (sb-ext:process-wait process)
         (sb-ext:process-close process))))))

(defun sb-process-input-stream (engine-client)
  "Return the input stream of the sb-process instance at the heart of
  \"engine-client\"."
  (sb-ext:process-input
   (ecase (prolog-engine-client-host engine-client)
     (:xsb (prolog-engine-client-process engine-client))
     (:scryer (scryer-client-process (prolog-engine-client-process engine-client))))))

(defun terminate-prolog-engine (engine-client)
  "Terminate the Prolog engine, first by halting the server by writing
\"end_of_file.\", and then by writing \"halt.\", to the process input
stream. Wait for the process to close after closing the process input
and output streams."
  (write-line "end_of_file." (prolog-engine-client-input-stream engine-client))
  (finish-output (prolog-engine-client-input-stream engine-client))

  (write-line "halt." (sb-process-input-stream engine-client))
  (finish-output (sb-process-input-stream engine-client))
  (close-prolog-engine-client engine-client))

(defun start-prolog-process (engine-client)
  "Launch the Prolog engine backend as a subprocess using the SBCL
run-program extension. Its use of an SBCL-specific extension makes it
non-portable to other Common Lisp implementations."
  (sb-ext:run-program (prolog-engine-client-path engine-client)
                      (ecase (prolog-engine-client-host engine-client)
                        ;; These command line arguments are needed to
                        ;; prevent junk output from clogging the XSB
                        ;; output stream, which is used by
                        ;; cl-psoatransrun to read answer bindings.
                        (:xsb '("--nobanner" "--quietload" "--noprompt" "--nofeedback"))
                        (:scryer '()))
                      :input :stream
                      :output :stream
                      :wait nil))

(defun connect-to-prolog-engine-socket (process)
  "Connect to a Prolog engine socket under the presumption that the
server prints its listener port to its standard output stream prior to
accepting socket connections. This is currently only true of
scryer_server.pl, as xsb_server.pl does not use sockets."
  (loop (handler-case
            (let* ((port (parse-integer (read-line (sb-ext:process-output process)))))
              (return (socket-connect "127.0.0.1" port)))
          (parse-error ()))))

(defun set-prolog-engine-client-stream (engine-client process)
  "Set the input-stream and output-stream slots of \"engine-client\"
according to whether the Prolog server communicates via process
streams or sockets. Also store \"process\" and its attendant socket
object (if one is necessary) available from the process slot of
\"engine-client\"."
  (with-slots ((process-slot process) input-stream output-stream)
      engine-client
    (ecase (prolog-engine-client-host engine-client)
      (:xsb (setf input-stream  (sb-ext:process-input process)
                  output-stream (sb-ext:process-output process)
                  process-slot  process))
      (:scryer (let ((socket (connect-to-prolog-engine-socket process)))
                 (setf input-stream  (socket-stream socket)
                       output-stream (socket-stream socket)
                       process-slot  (make-scryer-client
                                      :process process
                                      :socket socket)))))))
