(defun toggle-indent-tabs-mode ()
  (interactive)
  (setq indent-tabs-mode (not indent-tabs-mode)))

(defun swap-buffers ()
  (interactive)
  (when (eq (length (window-list-1)) 2)
    (let ((current-buf (current-buffer))
          another-buf)
      (other-window 1)
      (setq another-buf (current-buffer))
      (switch-to-buffer current-buf)
      (other-window 1)
      (switch-to-buffer another-buf)
      (other-window 1))
    t))

(defun window-splitted-vertically-p ()
  (eq (window-height)
      (save-window-excursion
        (delete-window)
        (window-height))))

(defun window-splitted-horizontally-p ()
  (eq (window-width)
      (save-window-excursion
        (delete-window)
        (window-width))))

(defun resplit-window (&optional reverse)
  (interactive)
  (when (= (length (window-list-1)) 2)
    (let* ((splitted-vertically-p (window-splitted-vertically-p))
           (select-other-window-p
            (or
             (and splitted-vertically-p (zerop (window-left-column)))
             (and (not splitted-vertically-p) (not (eq (window-top-line) 1)))))
           another-buf)
      (when reverse
        (setq select-other-window-p (not select-other-window-p)))
      (other-window 1)
      (setq another-buf (current-buffer))
      (delete-window)
      (if splitted-vertically-p
          (split-window-vertically)
        (split-window-horizontally))
      (if select-other-window-p (other-window 1))
      (switch-to-buffer another-buf)
      (other-window 1))
    t))
