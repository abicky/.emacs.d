(when (require 'package nil t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (package-initialize))

(when (not noninteractive)  ; not executed with --script option?
  ;; Add load paths
  (let ((default-directory (expand-file-name "~/.emacs.d/vendor")))
    (when (file-exists-p default-directory)
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path)))

  (when (require 'init-loader nil t)
    (setq init-loader-byte-compile t
          init-loader-show-log-after-init nil
          init-loader-directory (expand-file-name "config" user-emacs-directory))
    (init-loader-load)))

(defadvice package--make-autoloads-and-compile (before update-list-and-apply-patch activate)
  "Update the package list and apply patches before compiling packages"
  (let* ((pkg-name (ad-get-arg 0))
         (pkg-dir (ad-get-arg 1))
         (user-dir (file-name-as-directory user-emacs-directory))
         (pkg-file (expand-file-name "packages" user-dir))
         (patch-dir (expand-file-name pkg-name (concat user-dir "patch"))))
    (when (file-exists-p patch-dir)
      (loop for patch in (directory-files patch-dir t "\\.patch\\'")
            for orig = (expand-file-name (file-name-base patch) pkg-dir)
            for cmd = (format "patch %s %s" orig patch)
            unless (zerop (call-process-shell-command cmd nil "*mylog*")) do
              (error "Failed to apply patch: %s" cmd)))
    (with-temp-buffer
      (when (file-exists-p pkg-file)
        (insert-file-contents pkg-file))
      (let ((lines (delete "" (split-string (buffer-string) "\n"))))
        (delete-region (point-min) (point-max))
        (setq lines (sort (add-to-list 'lines pkg-name) 'string<))
        (insert (mapconcat 'identity lines "\n") "\n")
        (write-file pkg-file)))
    ))
