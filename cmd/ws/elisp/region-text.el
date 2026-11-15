(let* ((no-header %s)
       (windows (cons (selected-window)
                      (let (res)
                        (dolist (f (visible-frame-list))
                          (when (null (frame-parent f))
                            (dolist (w (window-list f (quote no-minibuffer)))
                              (push w res))))
                        (nreverse res))))
       target-win)
  (while (and windows (null target-win))
    (let ((w (pop windows)))
      (with-current-buffer (window-buffer w)
        (when (and mark-active
                   (mark t)
                   (/= (mark t) (window-point w)))
          (setq target-win w)))))
  (unless target-win
    (user-error "No marked text"))
  (with-current-buffer (window-buffer target-win)
    (let* ((beg (min (mark t) (window-point target-win)))
           (end (max (mark t) (window-point target-win)))
           (text (buffer-substring-no-properties beg end))
           (last (if (eq (char-before end) ?\n) (1- end) end)))
      (if no-header
          text
        (format "--- %s:%d-%d ---\n%s%s"
                (if buffer-file-name
                    (abbreviate-file-name buffer-file-name)
                  (buffer-name))
                (line-number-at-pos beg t)
                (line-number-at-pos last t)
                text
                (if (string-suffix-p "\n" text) "" "\n"))))))
