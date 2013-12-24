;; Start Emacs client
(when (require 'server nil t)
  (unless (server-running-p)
    (server-start)))
