(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

(when (not noninteractive)  ; not executed with --script option?
  (when (require 'init-loader nil t)
    (setq init-loader-byte-compile t
          init-loader-show-log-after-init nil
          init-loader-directory (expand-file-name "config" user-emacs-directory))
    (init-loader-load))
  ;; Add load paths
  (let ((default-directory (expand-file-name "~/.emacs.d/vendor")))
    (add-to-list 'load-path default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
