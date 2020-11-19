
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
  (pending-query nil :type (or null string)) ;; A pending query, i.e., one that prompted a KB recompilation.
  process ;; An object representing the Prolog subprocess.
  )

(defstruct scryer-stream
  "A structure containing a Scryer Prolog process and a usocket
connected to that process."
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

(defun enqueue-query (engine-client query-string)
  (setf (prolog-engine-client-pending-query engine-client)
        query-string))

(defun dequeue-query (engine-client)
  (prog1 (prolog-engine-client-pending-query engine-client)
    (enqueue-query engine-client nil)))

(defun process-input-stream (engine-client)
  (with-accessors ((host prolog-engine-client-host)
                   (process prolog-engine-client-process))
      engine-client
    (ecase host
      (:xsb (sb-ext:process-input process))
      (:scryer (socket-stream (scryer-stream-socket process))))))

(defun process-output-stream (engine-client)
  (with-accessors ((host prolog-engine-client-host)
                   (process prolog-engine-client-process))
      engine-client
    (ecase host
      (:xsb (sb-ext:process-output process))
      (:scryer (socket-stream (scryer-stream-socket process))))))

(defun close-prolog-engine-client-stream (engine-client)
  (with-accessors ((host prolog-engine-client-host)
                   (process prolog-engine-client-process))
      engine-client
    (ecase host
      (:xsb
       (close (sb-ext:process-input process) :abort t)
       (close (sb-ext:process-output process) :abort t)

       (sb-ext:process-wait process)
       (sb-ext:process-close process))
      (:scryer
       (socket-close (scryer-stream-socket process))

       (close (sb-ext:process-input (scryer-stream-process process)) :abort t)
       (close (sb-ext:process-output (scryer-stream-process process)) :abort t)

       (sb-ext:process-wait (scryer-stream-process process))
       (sb-ext:process-close (scryer-stream-process process))))))

(defun sb-process-input-stream (engine-client)
  (sb-ext:process-input
   (ecase (prolog-engine-client-host engine-client)
     (:xsb (prolog-engine-client-process engine-client))
     (:scryer (scryer-stream-process (prolog-engine-client-process engine-client))))))

(defun terminate-engine-client (engine-client)
  (write-line "halt." (sb-process-input-stream engine-client))
  (finish-output (sb-process-input-stream engine-client))
  (close-prolog-engine-client-stream engine-client))

(defun connect-to-prolog-engine-socket (process)
  (loop (handler-case
            (let* ((port (parse-integer (read-line (sb-ext:process-output process)))))
              (return (socket-connect "127.0.0.1" port)))
          (parse-error ()))))

(defun set-prolog-engine-client-stream (engine-client process)
  (setf (prolog-engine-client-process engine-client)
        (ecase (prolog-engine-client-host engine-client)
          (:xsb process)
          (:scryer (make-scryer-stream
                    :process process
                    :socket (connect-to-prolog-engine-socket process))))))
