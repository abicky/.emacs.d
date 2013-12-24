(setenv "EDITOR" "emacsclient")
(setenv "GIT_PAGER" "cat")

(when (require 'multi-shell nil t)
  (setq multi-shell-command "/bin/bash")
  (setq multi-shell-buffer-name "bash")
  (setq multi-shell-revert-window-after-complete nil)
  (define-key shell-mode-map (kbd "C-c n") 'multi-shell-next)
  (define-key shell-mode-map (kbd "C-c p") 'multi-shell-prev))

(defun bash ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally)
    (other-window 1))
  (if (functionp 'multi-shell-new)
      (multi-shell-new)
    (shell)))

(defun shell-mode-init ()
  (process-send-string (get-buffer-process (current-buffer)) "alias emacs='emacsclient'\n"))
(add-hook 'shell-mode-hook 'shell-mode-init)

;; cf. https://github.com/szermatt/emacs-bash-completion
;; (autoload 'bash-completion-dynamic-complete
;;   "bash-completion"
;;   "BASH completion hook")
;;(bash-completion-setup)
;; (add-hook 'shell-dynamic-complete-functions 'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions 'bash-completion-dynamic-complete)
