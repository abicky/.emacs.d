(when (require 'helm-config nil t)
  (helm-mode 1)
  ;; keybindings for global-map are defined in keybinding.el
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; Disable helm in some functions
  ;; (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
  ;; (add-to-list 'helm-completing-read-handlers-alist '(write-file . nil))
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-file-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (setq helm-ff-smart-completion nil)
  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern))))))

  (defun helm-buffers-list-pattern-transformer (pattern)
    (if (equal pattern "")
        pattern
      ;; Escape '.' to match '.' instead of an arbitrary character
      (setq pattern (replace-regexp-in-string "\\." "\\\\." pattern))
      (let ((first-char (substring pattern 0 1)))
        (cond ((equal first-char "*")
               (concat " " pattern))
              ((equal first-char "=")
               (concat "*" (substring pattern 1)))
              (t
               pattern)))))

  (add-to-list 'helm-source-buffers-list
               '(pattern-transformer helm-buffers-list-pattern-transformer))
  )
