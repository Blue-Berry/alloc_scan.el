;;; test-matching.el --- Test file matching logic -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(message "Testing file matching logic...")

;; Test data - the actual allocations from the dump
(defvar test-allocations 
  '(("command/src/command.ml" 2583 16 40 3319)
    ("command/src/command.ml" 2592 16 43 3319)
    ("command/src/command.ml" 2594 6 15 1024)))

;; Test files that might be open in Emacs
(defvar test-files
  '("/home/liam/playground/alloc_scan/lib/alloc_scan.ml"
    "/home/user/project/command/src/command.ml"  
    "/tmp/command.ml"
    "command.ml"))

(message "Allocations to match:")
(dolist (alloc test-allocations)
  (message "  %s" alloc))

(message "\nTesting file matching:")
(dolist (current-file test-files)
  (message "\nCurrent file: %s" current-file)
  (let ((matches 0))
    (dolist (alloc test-allocations)
      (let ((filepath (nth 0 alloc)))
        (let ((suffix-match (string-suffix-p filepath current-file))
              (basename-match (string-suffix-p (file-name-nondirectory filepath)
                                             (file-name-nondirectory current-file))))
          (when (or suffix-match basename-match)
            (setq matches (1+ matches))
            (message "  ✓ MATCH: %s (suffix:%s basename:%s)" 
                     filepath suffix-match basename-match)))))
    (message "  Total matches: %d" matches)))

(message "\nTest complete")