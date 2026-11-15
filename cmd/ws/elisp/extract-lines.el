(save-excursion
  (goto-char (point-max))
  (forward-line (- %d))
  (buffer-substring-no-properties (point) (point-max)))
