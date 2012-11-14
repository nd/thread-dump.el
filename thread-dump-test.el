(require 'ert)
(require 'thread-dump)

(ert-deftest ert-parse-threads ()
  (with-thread-dump "test-data/thread-dump"
    (let ((threads (thread-dump-parse-current-buffer)))
      (should (listp threads))
      (should (= (length threads) 78))
      (let ((thread (car threads)))
        (should (string= (thread-dump-get-thread-name thread)
                         "AWT-EventQueue-0 12.0#IU-122.562, eap:true"))
        )
      threads)))

(defmacro with-thread-dump (file &rest body)
  (declare (indent 1))
  `(let ((buf (get-buffer-create "*threads-test-tmp*")))
     (with-current-buffer buf
       (find-file ,file)
       ,@body)
     (kill-buffer buf)))


(macroexpand '(with-thread-dump "test-data/1"
                    (message "ok")
                    ))
