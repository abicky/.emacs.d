;; Synchronize clipboard for Emacs without the window system
(cond ((eq system-type 'darwin)  ;; for Mac OS X
       (setq interprogram-cut-function
             (lambda (text &optional rest)
               (let* ((process-connection-type nil)
                      (proc (start-process "pbcopy" "*Messages*" "pbcopy")))
                 (process-send-string proc text)
                 (process-send-eof proc)
                 (process-send-eof proc))))
       (setq interprogram-paste-function
             (lambda ()
               (shell-command-to-string "pbpaste"))))
      ((getenv "DISPLAY")        ;; for Linux with X11
       (setq interprogram-cut-function
             (lambda (text &optional rest)
               (let* ((process-connection-type nil)
                      (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
                 (process-send-string proc text)
                 (process-send-eof proc))))
       (setq interprogram-paste-function
             (lambda ()
               (shell-command-to-string "xsel -b -o")))))
