(require 'cl)

(defconst thread-dump-overview-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "n") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "j") 'thread-dump-overview-show-next-thread)
    (define-key map (kbd "p") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "k") 'thread-dump-overview-show-prev-thread)
    (define-key map (kbd "RET") 'thread-dump-overview-show-thread)
    (define-key map (kbd "o") 'thread-dump-overview-show-thread)
    (define-key map (kbd "v") 'thread-dump-overview-visit-thread)
    (define-key map (kbd "h") 'thread-dump-overview-hide)
    (define-key map (kbd "H") 'thread-dump-overview-hide-with-same-stack)
    (define-key map (kbd "q") 'thread-dump-overview-quit)
    (define-key map (kbd "/") 'thread-dump-overview-filter)
    (define-key map (kbd "N") 'thread-dump-overview-open-next-dump)
    (define-key map (kbd "P") 'thread-dump-overview-open-prev-dump)
    map))

(defun thread-dump-overview-mode ()
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (delete-other-windows)
  (toggle-truncate-lines 1)
  (setq major-mode 'thread-dump-overview-mode
        mode-name "Thread-Dump-Overview")
  (use-local-map thread-dump-overview-mode-map)
  (run-hooks 'thread-dump-overview-mode-hook))


(defun thread-dump-open-dir (dir)
  (interactive "DThread dump directory: ")
  (let ((files (directory-files dir t directory-files-no-dot-files-regexp)))
    (thread-dump-open-file (car files))

    (make-variable-buffer-local 'thread-dump-files)
    (setq thread-dump-files files)

    (make-variable-buffer-local 'thread-dump-file-index)
    (setq thread-dump-file-index 0)))


(defun thread-dump-open-file (file)
  (interactive "FThread dump: ")
  (let ((threads (with-temp-buffer
                   (insert-file-contents file)
                   (thread-dump-parse-current-buffer))))
    (thread-dump-enter threads)
    (make-variable-buffer-local 'thread-dump-file)
    (setq thread-dump-file file)
    (setq header-line-format (list file))))


(defun thread-dump-enter (threads)
  (make-variable-buffer-local 'thread-dump-ow-cur-thread-line)
  (thread-dump-show-overview threads)
  (make-variable-buffer-local 'thread-dump-threads)
  (setq thread-dump-threads threads)
  (make-variable-buffer-local 'thread-dump-filter)
  (thread-dump-overview-mode))


(defun thread-dump-show-overview (threads)
  (let* ((buf (get-buffer-create "*thread-dump-overview*")))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (dolist (thread threads nil)
        (thread-dump-show-thread-header thread))
      (backward-delete-char 1))
    (goto-char (point-min))
    (switch-to-buffer buf)))

(defun thread-dump-overview-quit ()
  (interactive)
  (delete-other-windows)
  (bury-buffer))

(defun thread-dump-show-thread-header (thread)
  (insert (propertize (concat (thread-dump-get-thread-name thread) "\n")
                      'id (thread-dump-get-thread-id thread))))

(defun thread-dump-overview-next-thread ()
  (interactive)
  (unless (eq (point-max) (line-end-position))
    (next-line)))

(defun thread-dump-overview-prev-thread ()
  (interactive)
  (unless (eq (point-min) (line-beginning-position))
    (next-line -1)))

(defun thread-dump-overview-show-next-thread ()
  (interactive)
  (thread-dump-overview-next-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-prev-thread ()
  (interactive)
  (thread-dump-overview-prev-thread)
  (thread-dump-overview-visit-thread))

(defun thread-dump-overview-show-thread ()
  (interactive)
  (thread-dump-overview-visit-thread t))

(defun thread-dump-overview-visit-thread (&optional switch-to-details)
  (interactive)
  (thread-dump-highlight-cur-thread)
  (let* ((id (get-text-property (point) 'id))
         (thread (thread-dump-find-thread-by-id id))
         (file thread-dump-file)
         (buf (get-buffer-create "*thread-dump-details*"))
         (inhibit-read-only t))
    (set-buffer buf)
    (erase-buffer)
    (set (make-local-variable 'truncate-lines) t)
    (insert (thread-dump-get-thread-contents thread))
    (goto-char (point-min))
    (and file (setq header-line-format (list file)))

    (let* ((w (get-buffer-window buf))
           (cur-win (selected-window)))
      (if (and w switch-to-details)
          (select-window w)
        (unless w
          (split-window-right 60)
          (select-window (window-next-sibling))
          (switch-to-buffer buf)
          (unless switch-to-details
            (select-window cur-win)))))))

(defun thread-dump-highlight-cur-thread ()
  (let ((inhibit-read-only t))
    (when thread-dump-ow-cur-thread-line
      (save-excursion
        (goto-line thread-dump-ow-cur-thread-line)
        (put-text-property (point-at-bol) (point-at-eol) 'face 'default)))
    (setq thread-dump-ow-cur-thread-line (line-number-at-pos))
    (put-text-property (point-at-bol) (point-at-eol) 'face 'thread-dump-current-thread)))

(defun thread-dump-overview-open-next-dump ()
  (interactive)
  (when (and thread-dump-files
             thread-dump-file-index
             (< thread-dump-file-index (- (length thread-dump-files) 1)))
    (setq thread-dump-file-index (+ 1 thread-dump-file-index))
    (thread-dump-open-file (nth thread-dump-file-index thread-dump-files))
    (and thread-dump-filter (thread-dump-overview-filter thread-dump-filter))))

(defun thread-dump-overview-open-prev-dump ()
  (interactive)
  (when (and thread-dump-files
             thread-dump-file-index
             (> thread-dump-file-index 0))
    (setq thread-dump-file-index (- thread-dump-file-index 1))
    (thread-dump-open-file (nth thread-dump-file-index thread-dump-files))
    (and thread-dump-filter (thread-dump-overview-filter thread-dump-filter))))

(defun thread-dump-find-thread-by-id (id)
  (find id
        thread-dump-threads
        :test '(lambda (x y) (= x (cdr (assoc 'id y))))))

(defun thread-dump-overview-filter (term)
  (interactive "MFilter: ")
  (if (equal term "")
      (progn
        (thread-dump-show-overview thread-dump-threads)
        (setq thread-dump-filter nil))
    (let ((filtered (delq nil
                          (mapcar (lambda (x)
                                    (if (thread-dump-match term x)
                                        x
                                      nil))
                                  thread-dump-threads))))
      (thread-dump-show-overview filtered)
      (setq thread-dump-filter term))))

(defun thread-dump-match (term thread)
  (string-match term (thread-dump-get-thread-contents thread)))

(defun thread-dump-parse-current-buffer ()
  (save-restriction
    (save-excursion
      (goto-char (point-min))
      (let ((threads (list))
            (thread-id 0))
        (while (re-search-forward "^\"" nil t)
          (move-beginning-of-line 1)
          (setq threads
                (cons (thread-dump-parse-thread-at-point thread-id) threads))
          (setq thread-id (+ thread-id 1)))

        (sort threads '(lambda (t1 t2)
                         (string< (downcase (thread-dump-get-thread-name t1))
                                  (downcase (thread-dump-get-thread-name t2)))))))))

(defun thread-dump-parse-thread-at-point (thread-id)
  (let* ((thread-start (point))
         (name-start (or (search-forward "\"" (line-end-position) t) thread-start))
         (name-end (or (- (search-forward "\"" (line-end-position) t) 1) (line-end-position)))
         (state (thread-dump-parse-thread-state-at-point))
         (stack-start (thread-dump-get-stack-start-at-point))
         (thread-end (if (re-search-forward "^\n" nil t) (line-beginning-position 1) (point-max))))
    (list
       (cons 'id thread-id)
       (cons 'name (buffer-substring-no-properties name-start name-end))
       (cons 'start thread-start)
       (cons 'end thread-end)
       (cons 'contents (buffer-substring-no-properties thread-start thread-end))
       (cons 'state state)
       (cons 'stack (if stack-start (buffer-substring-no-properties stack-start thread-end) nil)))))

(defun thread-dump-parse-thread-state-at-point ()
  (if (re-search-forward "java.lang.Thread.State: \\b\\([a-zA-Z_]+\\)\\b" (line-end-position 2) t)
      (buffer-substring-no-properties (match-beginning 1) (match-end 1))
    nil))

(defun thread-dump-get-stack-start-at-point ()
  (if (re-search-forward "^\\( \\|\t\\)*at" (line-end-position 2) t)
      (line-beginning-position 1)
    nil))

(defun thread-dump-get-thread-name (thread)
  (cdr (assoc 'name thread)))

(defun thread-dump-get-thread-id (thread)
  (cdr (assoc 'id thread)))

(defun thread-dump-get-thread-contents (thread)
  (cdr (assoc 'contents thread)))

(defun thread-dump-get-thread-state (thread)
  (cdr (assoc 'state thread)))

(defun thread-dump-get-thread-stack (thread)
  (cdr (assoc 'stack thread)))

(defface thread-dump-current-thread
  '((t :underline t
       :weight bold))
  "Current thread face."
  :group 'thread-dump-faces)

(provide 'thread-dump)
