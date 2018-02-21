;;; org-fancy-priorities.el --- Display org priorities as custom strings
;;
;; Copyright (C) 2018 Harry Bournis
;;
;; Author: Harry Bournis <harrybournis@gmail.com>
;; Created: 5 Feb 2018
;; Version: 1.0
;; Keywords: convenience faces outlines
;; Homepage: https://github.com/harrybournis/org-fancy-priorities
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;; Commentary:
;;
;; Org mode is great.  It is powerful, versatile and customizable.  Unfortunately,  I
;; always found the task priorities functionality a bit underwhelming, not in
;; terms of usability, but more in the visual department.
;;
;; Inspired by https://github.com/sabof/org-bullets, I created a
;; minor mode that displays org priorities as custom strings.  This mode does
;; NOT change your files in any way, it only displays the priority part of a
;; heading as your preferred string value.
;;
;; Set the org-fancy-priorities-list variable either with a list of strings in descending
;; priority importance, or an alist that maps each priority character to a custom string.
;;
;; (setq org-fancy-priorities-list '("HIGH" "MID" "LOW" "OPTIONAL"))
;;
;; or
;;
;; (setq org-fancy-priorities-list '((?A . "❗")
;;                                  (?B . "⬆")
;;                                  (?C . "⬇")
;;                                  (?D . "☕")
;;                                  (?1 . "⚡")
;;                                  (?2 . "⮬")
;;                                  (?3 . "⮮")
;;                                  (?4 . "☕")
;;                                  (?I . "Important")))
;;
;;; Code:

(eval-when-compile
  (require 'org))

(defgroup org-fancy-priorities nil
  "Display org priorities as custom strings"
  :group 'org-appearance
  :version "0.1")

(defcustom org-fancy-priorities-list
  '("❗" "⬆" "⬇" "☕")
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
    ;; Check if org-fancy-priorities-list is a list of strings or alists
    (cond ((equal 'string (type-of (car org-fancy-priorities-list)))
           (let ((index (- priority-int org-highest-priority)))
             (if (< index (length org-fancy-priorities-list))
                 (nth index org-fancy-priorities-list)
               (format "[#%s]" priority))))

          ((equal 'cons (type-of (car org-fancy-priorities-list)))
           (let ((value (cdr (assq priority-int org-fancy-priorities-list))))
             (if value
                 value
               (format "[#%s]" priority))))

          (t (display-warning '(org-fancy-priorities) "Invalid org-fancy-priorities-list value" :error)))))

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
    (with-no-warnings (font-lock-fontify-buffer))))

(provide 'org-fancy-priorities)

;;; org-fancy-priorities.el ends here
