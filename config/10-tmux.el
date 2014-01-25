(when (and (server-running-p)
           (getenv "TMUX"))
  (defvar tmux-window-conf nil)
  (defvar tmux-current-buffer nil)
  (defvar tmux-current-window
    (shell-command-to-string "tmux display-message -p '#W' | tr -d '\\n'"))
  (defvar tmux-after-restore-window-conf-hook nil)
  (defvar tmux-current-directory nil)
  (defvar tmux-invoked-from-tmux nil)

  (defun tmux-zoomed-p ()
    (equal (shell-command-to-string "tmux display-message -p '#F' | tr -d '\\n'") "*Z"))

  (defun tmux-other-window (&optional count)
    (interactive "p")
    (if (tmux-zoomed-p)
        (other-window (or count 1))
      (call-process-shell-command "tmux select-pane -t :.+")))

  (defun tmux-toggle-zoom ()
    (call-process-shell-command "tmux resize-pane -Z"))

  ;; the return value of `current-buffer' will be "*server*"
  ;; if `tmux-restore-window-conf' is called by emacsclient,
  ;; so save the current window regularly
  (defun tmux-set-current-buffer ()
    (unless (window-minibuffer-p)
      (setq tmux-current-buffer (current-buffer))))
  (run-with-idle-timer 0.1 0.1 'tmux-set-current-buffer)

  (defun tmux-restore-window-conf ()
    (when (tmux-zoomed-p)
      (let ((current-buf (current-buffer)))
        (cond ((= (length (window-list-1)) 2)
               (when (window-splitted-horizontally-p)
                 (resplit-window t)))
              (t (set-window-configuration tmux-window-conf)
                 (switch-to-buffer tmux-current-buffer))))
      (run-hooks 'tmux-after-restore-window-conf-hook)))

  (defun tmux-get-in-and-out-zsh ()
    (interactive)
    (when (equal tmux-current-window (getenv "EMACS_WINDOW"))
      (cond ((tmux-zoomed-p)
             (when (window-left (selected-window))
               (swap-buffers))
             (setq tmux-window-conf (current-window-configuration))
             (delete-other-windows)
             (tmux-toggle-zoom)
             (tmux-other-window))
            (t
             (tmux-toggle-zoom)
             ;; the width of the windows will be half the expected width
             ;; if call `tmux-restore-window-conf' immediatelly
             (run-at-time 0.01 nil 'tmux-restore-window-conf)))))

  (defun tmux-find-file (dir)
    (when (equal tmux-current-window (getenv "EMACS_WINDOW"))
      (setq tmux-current-directory dir)
      (setq tmux-invoked-from-tmux t)
      (add-hook 'tmux-after-restore-window-conf-hook 'tmux--invoke-find-file)
      (run-at-time 0.01 nil 'tmux-restore-window-conf)))

  (defun tmux--invoke-find-file ()
    (remove-hook 'tmux-after-restore-window-conf-hook 'tmux--invoke-find-file)
    (helm-find-files-1 tmux-current-directory))

  (defun tmux--clear-invoked-from-tmux ()
    (setq tmux-invoked-from-tmux nil))

  (defadvice helm-keyboard-quit (before restore-zsh activate)
    (when tmux-invoked-from-tmux
      ;; "Can't expand minibuffer to full frame" error occurs
      ;; if call `tmux-get-in-and-out-zsh' immediately
      (run-at-time 0.01 nil 'tmux-get-in-and-out-zsh)))

  (add-hook 'helm-cleanup-hook 'tmux--clear-invoked-from-tmux)
  (define-key global-map (kbd "C-x t") 'tmux-get-in-and-out-zsh)
  (define-key global-map [remap other-window] 'tmux-other-window)
  )
