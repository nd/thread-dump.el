(require 'ert)
(require 'thread-dump)

;(ert-deftest ert-parse-threads ()
;  (with-thread-dump "test-data/thread-dump"
;    (let ((threads (thread-dump-parse-current-buffer)))
;      (should (listp threads))
;      (should (= (length threads) 78))
;      (let ((t1 (car threads)))
;        (should (string= (thread-dump-get-thread-name t1) "Action Updater"))
;        (should (string= (thread-dump-get-thread-state t1) "WAITING"))
;        )
;      threads)))

(ert-deftest ert-parse-thread ()
  (with-thread "test-data/t1"
    (let ((thread (thread-dump-parse-thread-at-point 1)))
      (should (string= (thread-dump-get-thread-name thread) "ApplicationImpl pooled thread 311"))
      (should (string= (thread-dump-get-thread-state thread) "TIMED_WAITING")))))

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
