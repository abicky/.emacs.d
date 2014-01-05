;; Ruby

;; Indentation
(setq ruby-deep-indent-paren-style nil)
;; cf. http://stackoverflow.com/questions/7961533/emacs-ruby-method-parameter-indentation
(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))

;; For ruby-end
(defun ruby-end-after-load ()
  ;; Don't insert 'end' when space key is pressed
  (define-key ruby-end-mode-map (read-kbd-macro ruby-end-expand-spc-key) nil)
  ;; Overwrite `ruby-end-expand-p'
  (defadvice ruby-end-expand-p (around ruby-end-expand-p-around activate)
    "Overwrite `ruby-end-expand-p' to insert 'end'
when `ruby-end-expand-spc-key' is disabled and return key is pressed"
    (let (beginning-word beginning-indent end-word end-indent)
      (save-excursion
        (newline)
        (ruby-beginning-of-block)
        (setq beginning-word (current-word))
        (setq beginning-indent (current-indentation))
        (ruby-end-of-block)
        (setq end-word (current-word))
        (setq end-indent (current-indentation)))
      ;; delete LF inserted by (newline)
      (delete-char 1)
      (setq ad-return-value
            (and
             (or
              (not (equal end-word "end"))
              ;; the word in the end of the block is 'end' but it is for another block
              (not (equal beginning-indent end-indent)))
             (member beginning-word ruby-block-beg-keywords)))))
  )
(eval-after-load "ruby-end" '(ruby-end-after-load))


(defun ruby-mode-init ()
  (electric-pair-mode t)
  (electric-indent-mode t)
  (add-to-list 'electric-indent-chars ? )  ; indent also when space key is pressed
  (ruby-end-mode t)
  (remove-hook 'before-save-hook 'ruby-mode-set-encoding 'local)

  ;; For RSpec
  (when (string-match-p "_spec\\'" (file-name-base))
    (let ((rspec-keywords '("describe" "it" "context")))
      (set (make-local-variable 'ruby-block-beg-keywords)
           (append ruby-block-beg-keywords rspec-keywords))
      (set (make-local-variable 'ruby-block-beg-re) (regexp-opt ruby-block-beg-keywords))
      (font-lock-add-keywords nil (list (regexp-opt rspec-keywords 'words)))))

  ;; For adviced ruby-move-to-block
  (setq ruby-block-keywords
        (append ruby-block-beg-keywords ruby-block-mid-keywords '("end")))
  (setq ruby-block-keyword-re (regexp-opt ruby-block-keywords 'symbols))
  )

(add-hook 'ruby-mode-hook 'ruby-mode-init)
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))

(defadvice ruby-move-to-block (around ruby-move-to-block-fixed activate)
  "Move only one block. For example, when 'if' expression has 'else' block,
jump to the beginning of 'else' block by executing (ruby-move-to-block 1)
as shown in the following code:

if condition
  # jump to 'else' expression when execute (ruby-move-to-block 1)
  puts 'foo'
else
  puts 'bar'
end
"
  (let ((current-block-indent (ruby-calculate-indent))
        current-line-indent done)
    (back-to-indentation)
    (unless (looking-at ruby-block-keyword-re)
      (setq current-block-indent (- current-block-indent ruby-indent-level)))
    (while (and (not done) (not (if (< n 0) (bobp) (eobp))))
      (forward-line n)
      (cond
       ;; skip empty lines
       ((looking-at "^\\s *$"))
       ;; skip comments
       ((looking-at "^\\s *#"))
       ((and (> n 0) (looking-at "^=begin\\>"))
        (re-search-forward "^=end\\>"))
       ((and (< n 0) (looking-at "^=end\\>"))
        (re-search-backward "^=begin\\>"))
       (t
        (setq current-line-indent (current-indentation))
        (when (>= current-block-indent current-line-indent)
          (setq done t)))))
    (back-to-indentation)))
