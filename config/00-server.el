;; Start Emacs client
(when (require 'server nil t)
  (when (getenv "TMUX")
    ;; You should write the following code in your .zshrc
    ;; if [ "$TMUX" ]; then
    ;;     export EMACS_SERVER="server-$(tmux display -p '#S')"
    ;;     export EMACS_WINDOW=emacs
    ;;     export EDITOR="emacsclient -s ${EMACS_SERVER}"
    ;;     alias emacsclient=$EDITOR
    ;; fi
    (defvar server-tmux-client-alist nil)
    (defun server-select-server-pane ()
      (let* ((win-pane-str (shell-command-to-string
                            "tmux display-message -p '#I:#P' | tr -d '\\n'"))
             (win-pane-list (split-string win-pane-str ":"))
             (zoomed-p (string-match "Z"
                                     (shell-command-to-string
                                      (format "tmux display-message -t %s -p '#F'"
                                              (getenv "EMACS_WINDOW")))))
             (cmd (format "tmux select-window -t %s && tmux select-pane -t 0 %s"
                          (getenv "EMACS_WINDOW")
                          (if zoomed-p
                              ;; the active pane will be unzoomed when a pane is selected,
                              ;; so zoom again
                              "&& tmux resize-pane -Z"
                            ""))))
        (add-to-list 'server-tmux-client-alist
                     (cons (current-buffer) win-pane-list))
        (call-process-shell-command cmd)))

    (defun server-select-client-pane ()
      (let* ((win-pane-list (assoc-default (current-buffer) server-tmux-client-alist))
             (cmd (apply 'format
                         (append
                          '("tmux select-window -t %s && tmux select-pane -t %s")
                          win-pane-list))))
        (call-process-shell-command cmd)))

    (setq server-name (or (getenv "EMACS_SERVER") server-name))
    (add-hook 'server-visit-hook 'server-select-server-pane)
    (add-hook 'server-done-hook 'server-select-client-pane))

  (unless (server-running-p)
    (server-start)))
