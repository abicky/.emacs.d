;; Start Emacs client
(when (require 'server nil t)
  ;; You should write the following code in your .bashrc
  ;; if [ "$EMACS" ]; then
  ;;     alias emacs='emacsclient -s ${EMACS_SERVER:-server}'
  ;; fi
  (setq server-name (format "server-%s" (emacs-pid)))
  (setenv "EMACS_SERVER" server-name)
  (setenv "EDITOR" (concat "emacsclient -s " server-name))
  (unless (server-running-p)
    (server-start)))
