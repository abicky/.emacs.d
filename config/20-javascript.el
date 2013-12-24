;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; (setq js2-rebind-eol-bol-keys nil)
;; (add-hook 'js2-mode-hook
;;           (lambda()
;;             (setq js2-basic-offset 4)
;;             (setq tab-width 4)))

(eval-after-load "nodejs-repl"
  '(setq nodejs-repl-prompt "node> "))
