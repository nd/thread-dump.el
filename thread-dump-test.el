(require 'ert)
(require 'thread-dump)

(ert-deftest ert-parse-threads ()
  (with-thread-dump "test-data/thread-dump2"
    (let ((threads (thread-dump-parse-current-buffer)))
      (should (listp threads))
      (should (= (length threads) 2)))))

(ert-deftest ert-parse-thread ()
  (with-thread "test-data/t1"
    (let ((thread (thread-dump-parse-thread-at-point 1)))
      (should (string= (thread-dump-get-thread-name thread) "ApplicationImpl pooled thread 311"))
      (should (string= (thread-dump-get-thread-state thread) "TIMED_WAITING"))
      (let* ((stack-str (thread-dump-get-thread-stack thread))
             (stack (split-string stack-str "\r?\n")))
        (should (string-match
                 "\\( \\|\t\\)*at com.intellij.openapi.application.impl.ApplicationImpl$1$1.run(ApplicationImpl.java:154)"
                 (car (last stack))))
        (should (string-match
                 "\\( \\|\t\\)*at sun.misc.Unsafe.park(Native Method)"
                 (car stack)))
        (should (= 11 (length stack)))))))

(defmacro with-thread-dump (file &rest body)
  (declare (indent 1))
  `(let ((buf (get-buffer-create "*threads-test-tmp*")))
     (with-current-buffer buf
       (find-file ,file)
       ,@body)
     (kill-buffer buf)))

(defmacro with-thread (file &rest body)
  (declare (indent 1))
  `(let ((buf (get-buffer-create "*threads-test-tmp*")))
     (with-current-buffer buf
       (find-file ,file)
       (goto-char (point-min))
       ,@body)
     (kill-buffer buf)))
