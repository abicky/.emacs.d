(setenv "EDITOR" "emacsclient")

(defvar multi-term-buffer-p nil)
(defvar multi-term-debug-p nil)
(defvar multi-term-watch-timer nil)
(defvar multi-term-watch-interval 0.1)
(defvar multi-term-history-file (expand-file-name "~/.bash_history"))
(defvar multi-term-history-modified-time nil)
(defvar multi-term-source-name "Multi Term History")
(defvar multi-term-history-source
  `((name . ,multi-term-source-name)
    (init . term-update-history-candidates)
    (candidates-in-buffer)
    (action . term-send-raw-string)))

(defun bash ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally)
    (other-window 1))
  (multi-term))

(defadvice multi-term-internal (after set-up-variables activate)
  "Make `term-prompt-regexp' a local variable
and set the flag to identify if the buffer is created by `multi-term'"
  (set (make-local-variable 'term-prompt-regexp) ".*$ ")
  ;; Set a flag to identify if the buffer is opened by `multi-term'
  (set (make-local-variable 'multi-term-buffer-p) t)
  (set (make-local-variable 'show-trailing-whitespace) nil)
  (unless multi-term-watch-timer
    (setq multi-term-watch-timer
          (run-with-idle-timer
           multi-term-watch-interval multi-term-watch-interval
           'multi-term-update-history-candidates)))
  )

(defadvice term-send-raw (around my-term-send-raw activate)
  "Send keys only if the buffer is created by `multi-term'
and the cursor is in the last line of the terminal"
  (if (not multi-term-buffer-p)
      ad-do-it
    (let ((keys (this-command-keys))
          (send-keys-p (>= (point)
                           (process-mark (get-buffer-process (current-buffer))))))
      (cond ((equal keys "\C-y")
             (yank)
             (setq keys (current-kill 0)))
            ((equal keys "\C-k")
             (kill-line))
            ((equal keys "\C-w")
             (kill-region (point) (mark)))
            ((equal keys "\ed")
             (kill-word nil))
            ((equal keys "\C-@")
             (setq this-command 'set-mark-command)
             (set-mark-command nil))
            (send-keys-p
             ;; behavior of the original function
             (setq keys (string (aref keys (1- (length keys))))))
            (t
             (let ((original-command (lookup-key (current-global-map) keys)))
               (and multi-term-debug-p (message "execute: (%s 1)" original-command))
               (cond ((memq original-command '(keyboard-quit scroll-up-command))
                      (funcall original-command))
                     (t
                      (funcall original-command 1))))))
      (when send-keys-p
        (and multi-term-debug-p (message "send key: %s" keys))
        (term-send-raw-string keys)))))

(defun multi-term-recenter-and-reset-marker ()
  (interactive)
  (recenter-top-bottom)
  (setq term-current-row nil)
  (set-marker term-home-marker (window-start)))

(defun multi-term-search-history ()
  (interactive)
  (helm :sources 'multi-term-history-source))

(defun multi-term-update-history-candidates ()
  "Update candidate buffer if the history file is modified"
  (if (and (zerop (length (multi-term-list)))
           (not (multi-term-dedicated-exist-p)))
      ;; Cancel timer if there is no terminal
      (setq multi-term-watch-timer (cancel-timer multi-term-watch-timer))
    (let ((current-modified-time (nth 5 (file-attributes multi-term-history-file)))
          (helm-source-name multi-term-source-name))
      (when (not (equal current-modified-time multi-term-history-modified-time))
        (setq multi-term-history-modified-time current-modified-time)
        (with-current-buffer (helm-candidate-buffer 'global)
          (delete-region (point-min) (point-max))
          (insert-file-contents multi-term-history-file)
          (reverse-region (point-min) (point-max)))))))

(defun multi-term-after-load ()
  (setq multi-term-buffer-name (file-name-base (getenv "SHELL")))
  (add-to-list 'term-bind-key-alist '("C-l" . multi-term-recenter-and-reset-marker) t)
  (add-to-list 'term-bind-key-alist '("M-d" . term-send-raw) t)
  (add-to-list 'term-bind-key-alist '("M-r" . multi-term-search-history) t)
  (setq term-unbind-key-list (delete "C-h" term-unbind-key-list))
  (setq term-unbind-key-list (delete "C-y" term-unbind-key-list))
  (add-to-list 'term-unbind-key-list "C-t"))
(eval-after-load "multi-term" '(multi-term-after-load))
