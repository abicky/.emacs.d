(defun electric-after-load ()
  ;; Make `electric-indent-chars' a buffer local variable
  ;; to enable `electric-indent-mode' only in specific major modes
  (set-default (make-variable-buffer-local 'electric-indent-chars) nil)

  (defadvice electric-pair-post-self-insert-function
    (around electric-pair-post-self-insert-function-around activate)
    "Don't insert the closing pair in comments or strings"
    (unless (nth 8 (save-excursion (syntax-ppss (1- (point)))))
      ad-do-it))
  )
(eval-after-load "electric" 'electric-after-load)
