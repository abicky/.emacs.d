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

(defvar multi-term-unbind-key-list
  '("C-@" "C-t" "C-u"))

(defvar multi-term-bind-key-alist
  '(("C-b" . multi-term-backward-char)
    ("C-e" . multi-term-move-end-of-line)
    ("C-f" . multi-term-forward-char)
    ("C-h" . term-send-raw)
    ("C-k" . multi-term-kill-line)
    ("C-l" . multi-term-recenter-and-reset-marker)
    ("C-n" . multi-term-next-line)
    ("C-w" . multi-term-kill-region)
    ("C-y" . multi-term-paste)
    ("M->" . multi-term-end-of-buffer)
    ("M-b" . multi-term-backward-word)
    ("M-d" . multi-term-kill-word)
    ("M-f" . multi-term-forward-word)
    ("M-r" . multi-term-search-history)))

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
  (set (make-local-variable 'multi-term-point-at-bol) nil)
  (set (make-local-variable 'multi-term-point-at-prompt) nil)
  (setq term-command-hook 'multi-term--command-hook)
  (define-key term-raw-map [remap term-send-raw] 'multi-term-send-raw)
  (unless multi-term-watch-timer
    (setq multi-term-watch-timer
          (run-with-idle-timer
           multi-term-watch-interval multi-term-watch-interval
           'multi-term-update-history-candidates)))
  (when multi-term-debug-p
    (term-start-output-log
     (format "*Log <%s>*" (buffer-name (current-buffer)))))
  )

(defun multi-term-paste ()
  (interactive)
  (let ((point-state (multi-term--get-current-point-state)))
    (cond ((eq point-state 'in-line)
           (term-paste))
          ((eq point-state 'in-prompt)
           (message "Read only"))
          (t
           (yank)))))

(defun multi-term-kill-line ()
  (interactive)
  (let ((point-state (multi-term--get-current-point-state)))
    (cond ((eq point-state 'in-line)
           ;; "one line" of `term-mode' sometimes straddles multiple lines,
           ;; so use `kill-new'
           (let ((beg (point))
                 ;; `multi-term' can have extra whitespaces in the end of the buffer
                 (end (save-excursion
                        (+ (goto-char (point-max)) (skip-syntax-backward " ")))))
             (kill-new (replace-regexp-in-string
                        "\n" "" (filter-buffer-substring beg end))))
           (term-send-raw-string "\C-k"))
          ((eq point-state 'in-prompt)
           (message "Read only"))
          (t
           (kill-line)))))

(defun multi-term-kill-region ()
  (interactive)
  (let ((point-state (multi-term--get-current-point-state)))
    (cond ((eq point-state 'in-line)
           (if (< (mark) multi-term-point-at-prompt)
               (message "Read only")
             (let ((beg (point))
                   (end (mark)))
               (copy-region-as-kill beg end)
               (term-send-raw-string
                (if (> beg end)
                    (make-string (- beg end) ?\C-h)
                  (make-string (- end beg) ?\C-d))))))
          ((eq point-state 'in-prompt)
           (message "Read only"))
          (t
           (kill-region (point) (mark))))))

(defun multi-term-kill-word ()
  (interactive)
  (let ((point-state (multi-term--get-current-point-state)))
    (if (eq point-state 'in-prompt)
        (message "Read only")
      (kill-word nil)
      (when (eq point-state 'in-line)
        (term-send-forward-kill-word)))))

(defun multi-term-move-end-of-line (arg)
  (interactive "^p")
  (let ((point-state (multi-term--get-current-point-state)))
    (if point-state
        (term-send-raw-string "\C-e")
      (move-end-of-line (or arg 1)))))

(defun multi-term-send-raw ()
  "Send keys only if the buffer is created by `multi-term'
and the cursor is in the last line of the terminal"
  (interactive)
  (let* ((keys (this-command-keys))
         (point-state (multi-term--get-current-point-state))
         (send-keys-p (eq point-state 'in-line)))
    (cond (send-keys-p
           ;; behavior of the original function
           (setq keys (string (aref keys (1- (length keys)))))
           (and multi-term-debug-p (message "send key: %s" keys))
           (term-send-raw-string keys))
          (t
           (let ((original-command (lookup-key (current-global-map) keys)))
             (and multi-term-debug-p (message "execute: (%s 1)" original-command))
             (cond ((memq original-command '(keyboard-quit scroll-up-command))
                    (funcall original-command))
                   ((and (memq original-command '(self-insert-command delete-backward-char))
                         (eq point-state 'in-prompt))
                    (message "Read only"))
                   (t
                    (funcall original-command 1))))))))

(defun multi-term--command-hook (string)
  "Save points to determine if the cursor is in the last line of the terminal"
  (setq multi-term-point-at-bol (point))
  ;; Set a timer because prompt is not displayed yet
  (run-with-idle-timer 0.1 nil 'multi-term--set-prompt-point)
  (term-command-hook string))

(defun multi-term--set-prompt-point ()
  (setq multi-term-point-at-prompt
        (copy-marker (process-mark (get-buffer-process (current-buffer))))))

(defun multi-term--get-current-point-state ()
  (let ((pos (point)))
    (cond ((>= pos multi-term-point-at-prompt)
           'in-line)
          ((>= pos multi-term-point-at-bol)
           'in-prompt)
          (t
           nil))))

(defun multi-term-next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines only if it is not in the last line"
  (interactive "^p\np")
  (if (multi-term--get-current-point-state)
      (message "Last line")
    (next-line (or arg 1))
    (when (eq (multi-term--get-current-point-state) 'in-line)
      ;; Move the cursor of the terminal
      (let ((beg (point-marker))
            (end (process-mark (get-buffer-process (current-buffer)))))
        (cond ((> beg end)
               (term-send-raw-string (make-string (- beg end) ?\C-f)))
              ((< beg end)
               (term-send-raw-string (make-string (- end beg) ?\C-b))))))))

(defun multi-term-forward-char (&optional n)
  (interactive "^p")
  ;; TODO: consider the case where N is greater than 1
  (if (eq (multi-term--get-current-point-state) 'in-line)
      (term-send-raw-string "\C-f")
    (forward-char n)
    (when (eq (multi-term--get-current-point-state) 'in-line)
      ;; Move the cursor of the terminal
      (term-send-raw-string "\C-a"))))

(defun multi-term-backward-char (&optional n)
  (interactive "^p")
  ;; TODO: consider the case where N is greater than 1
  (if (<= (point-marker) multi-term-point-at-prompt)
      (backward-char n)
    ;; Move the cursor of the terminal
    (term-send-raw-string "\C-b")))

(defun multi-term-forward-word (&optional arg)
  (interactive "^p")
  ;; TODO: consider the case where ARG is greater than 1
  (let ((point-state (multi-term--get-current-point-state)))
    (cond ((not point-state)
           (forward-word arg))
          ((or (eq point-state 'in-line)
               (save-excursion
                 (forward-word arg)
                 (eq (multi-term--get-current-point-state) 'in-line)))
           ;; Move the cursor of the terminal
           (term-send-forward-word))
          (t
           (forward-word arg)))))

(defun multi-term-backward-word (&optional arg)
  (interactive "^p")
  ;; TODO: consider the case where ARG is greater than 1
  (if (<= (point-marker) multi-term-point-at-prompt)
      (backward-word arg)
    ;; Move the cursor of the terminal
    (term-send-backward-word)))

(defun multi-term-end-of-buffer (&optional arg try-vscroll)
  "Move cursor to the end of the last line"
  (interactive)
  (push-mark)
  (term-send-raw-string "\C-e"))

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
  (setq term-unbind-key-list
        (append term-unbind-key-list multi-term-unbind-key-list))
  (setq term-bind-key-alist
        (append term-bind-key-alist multi-term-bind-key-alist)))
(eval-after-load "multi-term" '(multi-term-after-load))
