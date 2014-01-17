(when (and (server-running-p)
           (getenv "TMUX"))
  (defvar tmux-window-conf nil)
  (defvar tmux-current-buffer nil)
  (defvar tmux-current-window
    (shell-command-to-string "tmux display-message -p '#W' | tr -d '\\n'"))

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
    (setq tmux-current-buffer (current-buffer)))
  (run-with-idle-timer 0.1 0.1 'tmux-set-current-buffer)

  (defun tmux-restore-window-conf ()
    (when (tmux-zoomed-p)
      (let ((current-buf (current-buffer)))
        (set-window-configuration tmux-window-conf)
        (switch-to-buffer tmux-current-buffer))))

  (defun tmux-get-in-and-out-zsh ()
    (interactive)
    (when (equal tmux-current-window (getenv "EMACS_WINDOW"))
      (cond ((tmux-zoomed-p)
             (setq tmux-window-conf (current-window-configuration))
             (delete-other-windows)
             (tmux-toggle-zoom)
             (tmux-other-window))
            (t
             (tmux-toggle-zoom)
             ;; the width of the windows will be half the expected width
             ;; if call `tmux-restore-window-conf' immediatelly
             (run-at-time 0.01 nil 'tmux-restore-window-conf)))))

  (define-key global-map (kbd "C-x t") 'tmux-get-in-and-out-zsh)
  (define-key global-map [remap other-window] 'tmux-other-window)
  )