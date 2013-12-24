;; Don't show the startup splash screen
(setq inhibit-startup-message t)

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; Use unified diff format
(setq diff-switches "-u")

;; Use spaces for indentations except text-mode
(setq-default indent-tabs-mode nil)
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'toggle-indent-tabs-mode)

;; Disable Transient Mark mode
(transient-mark-mode 0)

;; Higlight matching parenthesis
(show-paren-mode t)

;; Show column number
(column-number-mode t)

;; Uniquify buffer names
(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; Log recently opened files
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Don't add duplicate commands to the history
(setq-default comint-input-ignoredups t)
;; Don't display the original command in the output
(setq-default comint-process-echoes t)
