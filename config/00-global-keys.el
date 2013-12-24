(define-key global-map (kbd "C-h")     'delete-backward-char)
(define-key global-map (kbd "C-c C-i") 'indent-region)
(define-key global-map (kbd "C-c i")   'imenu)
(define-key global-map (kbd "C-c r")   'rgrep)
(define-key global-map (kbd "C-c o")   'occur)
(define-key global-map (kbd "C-c ;")   'comment-or-uncomment-region)
(define-key global-map (kbd "M-?")     'help-for-help)

;; Assign 'find-function to C-x F
(find-function-setup-keys)

;; Moving windows
(define-key global-map (kbd "C-t")   'other-window)
;; (define-key global-map (kbd "M-t")
;;   '(lambda ()
;;      (interactive)
;;      (other-window -1)))
;; (windmove-default-keybindings)
;; (define-key global-map (kbd "C-c k") 'windmove-up)
;; (define-key global-map (kbd "C-c j") 'windmove-down)
;; (define-key global-map (kbd "C-c h") 'windmove-left)
;; (define-key global-map (kbd "C-c l") 'windmove-right)

;; Helm
(eval-after-load "helm-config"
  '(progn
     (define-key global-map (kbd "M-x")     'helm-M-x)
     (define-key global-map (kbd "C-x C-f") 'helm-find-files)
     (define-key global-map (kbd "C-x C-r") 'helm-recentf)
     (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
     (define-key global-map (kbd "C-c i")   'helm-imenu)
     (define-key global-map (kbd "C-x b")   'helm-buffers-list)
     ))
