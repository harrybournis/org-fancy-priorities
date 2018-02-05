;;; package --- Summary:

;;; Commentary:

;;; Code:
(eval-when-compile
  (require 'org))

(defgroup org-fancy-priorities nil
  "Display org priorities as custom strings"
  :group 'org-appearance
  :version "0.1")

(defcustom org-custom-priority-list
  '("⚡" "⬆" "⬇" "☕")
  ;; or
  ;; '((?A . "❗")
  ;;   (?B . "⬆")
  ;;   (?C . "⬇")
  ;;   (?D . "☕")
  ;;   (?1 . "❗")
  ;;   (?2 . "⮬")
  ;;   (?3 . "⮮")
  ;;   (?4 . "☠"))
  "The list of custom strings that will appear instead of the org mode defaults.
Like with org priorities, it starts with the highest priority and decreases in severity.

Note that you have to include the question mark before the character even if it is a
number, or you won't get the correct ascii value."
  :group 'org-fancy-priorities
  :type '(choice (repeat :tag "Same symbols for all files" (string))
                 (repeat :tag "Custom symbol for each priority value" (cons integer string))))

(defun org-fancy-priorities-get-value (priority)
  "Return the string that will appear instead of the PRIORITY arg.
Return nil if a value has not been specified for this priority.
  PRIORITY Is a string of just the priority value e.g. \"A\" \"B\" etc."
  (let ((priority-int (string-to-char priority)))
    ;; Check if org-custom-priority-list is a list of strings or alists
    (cond ((equal 'string (type-of (car org-custom-priority-list)))
           (let ((index (- priority-int org-highest-priority)))
             (if (< index (length org-custom-priority-list))
                 (nth index org-custom-priority-list)
               (format "[#%s]" priority))))

          ((equal 'cons (type-of (car org-custom-priority-list)))
           (let ((value (cdr (assq priority-int org-custom-priority-list))))
             (if value
                 value
               (format "[#%s]" priority))))

          (t (display-warning '(org-fancy-priorities) "Invalid org-custom-priority-list value" :error)))))

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
    (font-lock-flush)))

(provide 'org-fancy-priorities)

;;; org-fancy-priorities ends here
