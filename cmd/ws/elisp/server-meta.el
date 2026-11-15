(format "%d\t%s\t%s\t%s"
        (emacs-pid)
        (if (visible-frame-list) "yes" "no")
        (abbreviate-file-name default-directory)
        (buffer-name (window-buffer (selected-window))))
