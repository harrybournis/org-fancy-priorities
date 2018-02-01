(defgroup org-fancy-priorities nil
  "Display org priorities as custom strings"
  :group 'org-appearance
  :version "0.1")

(defcustom org-custom-priority-list
  '("HIGH" "MID" "LOW" "OPTIONAL")
  "The list of custom strings that will appear instead of
  the org mode defaults. Like with org priorities, it starts
  with the highest priority and decreases in severity."
  :group 'org-fancy-priorities
  :type '(repeat (string)))

(defun org-fancy-priorities-get-value (priority)
  "Returns the string that will appear instead of the PRIORITY arg,
  or nil if a value has not been specified for this priority.

  PRIORITY Is a string of just the priority value e.g. \"A\" \"B\" etc."
  (let ((index (- (string-to-char priority) org-highest-priority)))
    (if (< index (length org-custom-priority-list))
        (nth index org-custom-priority-list)
      (format "[#%s]" priority))))

;;;###autoload
(define-minor-mode org-fancy-priorities-mode
  "Customize the appearance of org-mode priorities.
  This mode does not alter your files in any way, it
  only changes the way that priorities are shown in your editor."
  nil " FancyPriorities" nil
  (let ((keyword '((".*?\\(\\[#\\([A-Z0-9]\\)\\] ?\\)"
                    (0 (progn
                         (let ((custom-priority (org-fancy-priorities-get-value (match-string 2))))
                           (put-text-property (match-beginning 1) (- (match-end 1) 1) 'display custom-priority)
                           nil)))))))
    (if org-fancy-priorities-mode
        (font-lock-add-keywords nil keyword)
      (progn
        (font-lock-remove-keywords nil keyword)
        (remove-text-properties (buffer-end -1) (buffer-end 1) '(display nil))))
    (font-lock-fontify-buffer)))

(provide 'org-fancy-priorities)
