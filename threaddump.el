(defvar thread-dump-mode-hook nil)

(defvar thread-dump-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for thread-dump major mode")

(defun thread-dump-mode ()
  "Major mode for viewing java thread dumps"
  (interactive)
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (setq major-mode 'thread-dump-mode
        mode-name "ThreadDump")
  (use-local-map thread-dump-mode-map)
  (run-hooks 'thread-dump-mode-hook))

(provide 'threaddump)
