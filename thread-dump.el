(defvar thread-dump-mode-hook nil)

(defconst thread-dump-mode-map
  (let ((map (make-keymap)))
    (define-key map "n" 'thread-dump-next-thread)
    (define-key map "p" 'thread-dump-prev-thread)
    (define-key map "j" 'thread-dump-next-thread)
    (define-key map "k" 'thread-dump-prev-thread)
    map)
  "Keymap for thread-dump major mode")

(defun thread-dump-mode ()
  "Major mode for viewing java thread dumps"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)

  (make-variable-buffer-local 'thread-dump-threads)
  (make-variable-buffer-local 'thread-dump-threads-number)
  (make-variable-buffer-local 'thread-dump-current-thread)
  (setq thread-dump-current-thread 0)

  (thread-dump-parse-buffer)
  (setq major-mode 'thread-dump-mode
        mode-name "Thread-Dump")
  (use-local-map thread-dump-mode-map)
  (run-hooks 'thread-dump-mode-hook))


(defun thread-dump-parse-buffer () 
  (setq thread-dump-threads '())
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\"" nil t)
        (let* ((thread-start (line-beginning-position 1)))
          (setq thread-dump-threads (cons thread-start thread-dump-threads))))))
  (setq thread-dump-threads (reverse thread-dump-threads))
  (setq thread-dump-threads-number (length thread-dump-threads)))


(defun thread-dump-next-thread ()
  (interactive)
  (if (< thread-dump-current-thread (- thread-dump-threads-number 1))
      (progn
        (incf thread-dump-current-thread)
        (let ((pos (nth thread-dump-current-thread thread-dump-threads)))
          (goto-char pos)))))

(defun thread-dump-prev-thread ()
  (interactive)
  (if (> thread-dump-current-thread 0)
      (progn 
        (incf thread-dump-current-thread -1)
        (let ((pos (nth thread-dump-current-thread thread-dump-threads)))
          (goto-char pos)))))

(provide 'thread-dump)
