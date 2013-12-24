(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))

(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let* ((code (buffer-string))
         (current-file (buffer-file-name))
         (ext (or (and (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s-*$" code)
                       (file-name-extension (match-string 1 code)))
                  "png"))  ; default ext is png
         (cmd (format "java -jar %s %s %s -t%s %s"
                      plantuml-java-options plantuml-jar-path
                      plantuml-options ext current-file)))
    (message cmd)
    (shell-command cmd)
    (message "Wrote %s" (expand-file-name (concat (file-name-base current-file) "." ext)
                                          (file-name-directory current-file)))))

(eval-after-load "plantuml-mode"
  '(progn
     (setq plantuml-jar-path     "/usr/local/jars/plantuml.jar"
           plantuml-java-options ""
           plantuml-options      "-charset UTF-8"
           plantuml-mode-map     (make-sparse-keymap))
     (define-key plantuml-mode-map (kbd "C-c C-c") 'plantuml-execute)))
