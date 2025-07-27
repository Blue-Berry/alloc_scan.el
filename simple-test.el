;;; simple-test.el --- Simple test for debugging

;; Test data - exact line from the dump file
(defvar test-line "      (alloc{command/src/command.ml:2583,16-40} 3319")

;; Test regex pattern
(defvar test-pattern "(alloc{\\([^}]+\\)} \\([0-9]+\\)")

(message "Testing regex pattern...")
(message "Pattern: %s" test-pattern)
(message "Test line: %s" test-line)

(if (string-match test-pattern test-line)
    (progn
      (message "SUCCESS! Regex matches")
      (message "  Group 1 (location): %s" (match-string 1 test-line))
      (message "  Group 2 (blocks): %s" (match-string 2 test-line)))
  (message "FAILED! Regex does not match"))

;; Test parsing the location part
(when (string-match test-pattern test-line)
  (let ((location (match-string 1 test-line)))
    (message "Parsing location: %s" location)
    (when (string-match "\\([^:]+\\):\\([0-9]+\\),\\([0-9]+\\)-\\([0-9]+\\)" location)
      (message "  File: %s" (match-string 1 location))
      (message "  Line: %s" (match-string 2 location))
      (message "  Col start: %s" (match-string 3 location))
      (message "  Col end: %s" (match-string 4 location)))))

(message "Test complete")