(let ((text ""))
  (dolist (frame %s)
    (when (null (frame-parent frame))
      (dolist (window (window-list frame (quote no-minibuffer)))
        (with-current-buffer (window-buffer window)
          (setq text (concat text
                             (format "\n--- Buffer: %%s ---\n" (buffer-name))
                             %s))))))
  text)
