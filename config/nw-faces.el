(custom-set-faces
 '(link              ((t (:foreground "color-75"))))       ; for Emacs manual
 '(link-visited      ((t (:foreground "brightmagenta"))))  ; for Emacs manual
 '(minibuffer-prompt ((t (:foreground "brightcyan"))))
 '(shadow            ((t (:foreground "white"))))          ; for diff
 ;; '(highlight ((t (:foreground "green"))))
 ;; '(info-node ((t (:foreground "brightgreen"))))
 ;; '(match     ((t (:background "yellow"))))

 ;; font-lock-mode
 '(font-lock-builtin-face       ((t (:foreground "color-198"))))  ; #include etc...
 '(font-lock-comment-face       ((t (:foreground "brightred"))))
 '(font-lock-constant-face      ((t (:foreground "cyan"))))
 '(font-lock-function-name-face ((t (:foreground "color-75" :weight bold))))
 '(font-lock-keyword-face       ((t (:foreground "color-159"))))
 '(font-lock-string-face        ((t (:foreground "color-213"))))
 '(font-lock-type-face          ((t (:foreground "brightgreen"))))
 '(font-lock-variable-name-face ((t (:foreground "yellow"))))

 ;; diff-mode
 '(diff-added        ((t (:foreground "brightgreen"))))
 '(diff-added-face   ((t nil)) t)
 '(diff-file-header  ((t (:weight bold))))
 '(diff-header       ((t (:background "brightblack"))))
 '(diff-removed      ((t (:foreground "brightred"))))
 '(diff-removed-face ((t nil)) t)

 ;; custom
 '(custom-group-tag    ((t (:inherit variable-pitch :foreground "brightblue"
                                     :weight bold :height 1.2))))
 '(custom-state        ((t (:foreground "green"))))
 '(custom-variable-tag ((t (:foreground "brightblue" :weight bold))))

 ;; comint-mode
 '(comint-highlight-prompt ((t (:foreground "brightwhite"))))

 ;; helm
 '(helm-ff-directory  ((t (:foreground "brightblue"))))
 '(helm-ff-executable ((t (:foreground "brightred"))))
 '(helm-ff-file       ((t (:foreground "brightwhite"))))
 '(helm-selection     ((t (:background "color-58" :underline t))))
 '(helm-visible-mark  ((t (:background "color-58"))))
 )
