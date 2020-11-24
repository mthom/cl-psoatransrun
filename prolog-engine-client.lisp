
(in-package #:prolog-engine-client)

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
  "A structure containing the name, path and socket of a Prolog
server, confusingly termed a client, which actually hosts a
server. The server compiles and loads the translated Prolog KB sent to
it by cl-psoatransrun. It receives query strings it evaluates against
the translated KB, and sends back answer strings."
  (host :scryer :type (or (eql :xsb) (eql :scryer))) ;; The name of the host system.
  (path #p"" :type pathname)
  process ;; An object representing the Prolog subprocess.
  input-stream
  output-stream)

(defstruct scryer-client
  "Represents the means of communicating with a Scryer Prolog server -- its subprocess
and socket objects."
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
run-program extension. Obviously this function isn't portable to other
Common Lisp implementations."
  (sb-ext:run-program (prolog-engine-client-path engine-client)
                      (ecase (prolog-engine-client-host engine-client)
                        ;; These command line arguments are needed to
                        ;; prevent junk output from clogging the XSB
                        ;; output stream, which is used by
                        ;; cl-psoatransrun to read solution sets.
                        (:xsb '("--nobanner" "--quietload" "--noprompt" "--nofeedback"))
                        (:scryer '()))
                      :input :stream
                      :output :stream
                      :wait nil))

(defun connect-to-prolog-engine-socket (process)
  (loop (handler-case
            (let* ((port (parse-integer (read-line (sb-ext:process-output process)))))
              (return (socket-connect "127.0.0.1" port)))
          (parse-error ()))))

(defun set-prolog-engine-client-stream (engine-client process)
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
