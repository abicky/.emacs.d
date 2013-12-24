(eval-after-load "electric"
  '(defadvice electric-pair-post-self-insert-function
     (around electric-pair-post-self-insert-function-around activate)
     "Don't insert the closing pair in comments or strings"
     (unless (nth 8 (save-excursion (syntax-ppss (1- (point)))))
       ad-do-it)))
