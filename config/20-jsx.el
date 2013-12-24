(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(autoload 'jsx-mode "jsx-mode" "JSX mode" t)

(eval-after-load "jsx-mode"
  '(setq jsx-use-auto-complete t
         jsx-syntax-check-mode "compile"
         jsx-use-flymake t))

(defun jsx-mode-init ()
  (define-key jsx-mode-map (kbd "C-c d") 'jsx-display-popup-err-for-current-line)
  (define-key jsx-mode-map (kbd "RET") 'newline-and-indent)
  (define-key jsx-mode-map (kbd "C-c TAB") 'jsx-auto-complete)
  (when (require 'auto-complete nil t)
    (auto-complete-mode t)))
(add-hook 'jsx-mode-hook 'jsx-mode-init)
