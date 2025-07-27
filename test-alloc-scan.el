;;; test-alloc-scan.el --- Tests for alloc-scan plugin -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive tests for the alloc-scan plugin to identify parsing issues

;;; Code:

(load-file (expand-file-name "alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Test configuration
(setq alloc-scan-debug t)

;; Test data - sample allocation line from real dump file
(defvar test-allocation-line 
  "      (alloc{command/src/command.ml:2583,16-40} 3319")

(defvar test-dump-content
  "some content before
      (alloc{command/src/command.ml:2583,16-40} 3319
        \"camlDune__exe__Main.anon_fn[command.ml:2583,16--40]_1897\"
        72057594037927941 version/1586)
    set_of_closures/1596
      (alloc{command/src/command.ml:2592,16-43} 3319
        \"camlDune__exe__Main.anon_fn[command.ml:2592,16--43]_1925\"
        72057594037927941 build_info/1585))
   (alloc{command/src/command.ml:2594,6-15} 1024
     (app{command/src/command.ml:2586,8-245} \"camlCommand.add_9329\" base/1594
some content after")

;;; Test Functions

(defun test-basic-regex ()
  "Test basic regex pattern matching."
  (message "\n=== Testing Basic Regex ===")
  (let ((pattern "(alloc{\\([^}]+\\)} \\([0-9]+\\)")
        (test-string test-allocation-line))
    (if (string-match pattern test-string)
        (progn
          (message "✓ Regex matches!")
          (message "  Location: %s" (match-string 1 test-string))
          (message "  Blocks: %s" (match-string 2 test-string)))
      (message "✗ Regex does not match"))))

(defun test-regex-on-content ()
  "Test regex on multi-line content."
  (message "\n=== Testing Regex on Multi-line Content ===")
  (let ((pattern "(alloc{\\([^}]+\\)} \\([0-9]+\\)")
        (content test-dump-content)
        (start 0)
        (matches 0))
    (while (string-match pattern content start)
      (setq matches (1+ matches))
      (message "Match %d: %s -> %s blocks" 
               matches 
               (match-string 1 content)
               (match-string 2 content))
      (setq start (match-end 0)))
    (message "Total matches found: %d" matches)))

(defun test-file-reading ()
  "Test reading the actual dump file."
  (message "\n=== Testing File Reading ===")
  (let ((dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
    (if (file-readable-p dump-file)
        (progn
          (message "✓ File is readable")
          (with-temp-buffer
            (insert-file-contents dump-file)
            (let ((content (buffer-string)))
              (message "  File size: %d characters" (length content))
              (if (string-match "(alloc{" content)
                  (message "  ✓ Contains alloc patterns")
                (message "  ✗ No alloc patterns found")))))
      (message "✗ File is not readable"))))

(defun test-parse-dump-file ()
  "Test the actual parse-dump-file function."
  (message "\n=== Testing parse-dump-file Function ===")
  (let ((dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump")
        (alloc-scan-debug t))
    (if (file-readable-p dump-file)
        (let ((result (alloc-scan--parse-dump-file dump-file)))
          (message "Parse result: %d allocations" (length result))
          (when result
            (message "First allocation: %s" (car result))
            (when (> (length result) 1)
              (message "Second allocation: %s" (cadr result)))))
      (message "✗ Cannot read dump file"))))

(defun test-file-matching ()
  "Test file matching logic."
  (message "\n=== Testing File Matching Logic ===")
  (let ((allocations '(("command/src/command.ml" 2583 16 40 3319)
                       ("lib/alloc_scan.ml" 100 5 20 1024)
                       ("bin/main.ml" 50 10 25 2048)))
        (test-files '("/home/liam/playground/alloc_scan/lib/alloc_scan.ml"
                      "/home/user/project/command/src/command.ml"
                      "/tmp/bin/main.ml")))
    (dolist (current-file test-files)
      (message "\nTesting with current file: %s" current-file)
      (dolist (alloc allocations)
        (let ((filepath (nth 0 alloc))
              (line (nth 1 alloc))
              (col-start (nth 2 alloc))
              (col-end (nth 3 alloc))
              (blocks (nth 4 alloc)))
          (let ((suffix-match (string-suffix-p filepath current-file))
                (basename-match (string-suffix-p (file-name-nondirectory filepath)
                                                (file-name-nondirectory current-file))))
            (message "  %s:%d,%d-%d -> suffix:%s basename:%s" 
                     filepath line col-start col-end suffix-match basename-match)))))))

(defun test-create-test-dump-file ()
  "Create a minimal test dump file."
  (message "\n=== Creating Test Dump File ===")
  (let ((test-file "/tmp/test-alloc.dump"))
    (with-temp-file test-file
      (insert test-dump-content))
    (message "Created test file: %s" test-file)
    test-file))

(defun test-with-minimal-dump ()
  "Test parsing with minimal dump file."
  (message "\n=== Testing with Minimal Dump File ===")
  (let ((test-file (test-create-test-dump-file))
        (alloc-scan-debug t))
    (let ((result (alloc-scan--parse-dump-file test-file)))
      (message "Minimal dump parse result: %d allocations" (length result))
      (dolist (alloc result)
        (message "  Allocation: %s" alloc)))))

(defun test-highlighting-logic ()
  "Test the highlighting logic."
  (message "\n=== Testing Highlighting Logic ===")
  (let ((test-allocations '(("test.ml" 5 10 20 1024)
                           ("other.ml" 3 5 15 2048))))
    ;; Create a test buffer
    (with-temp-buffer
      (insert "line 1\n")
      (insert "line 2\n") 
      (insert "line 3\n")
      (insert "line 4\n")
      (insert "line 5 - this should be highlighted\n")
      (insert "line 6\n")
      
      ;; Set buffer file name to match our test allocation
      (setq buffer-file-name "/tmp/test.ml")
      
      (message "Created test buffer with %d lines" (line-number-at-pos (point-max)))
      (message "Buffer file name: %s" buffer-file-name)
      
      ;; Test highlighting
      (alloc-scan--highlight-allocations test-allocations)
      (message "Created %d overlays" (length alloc-scan--overlays))
      
      ;; Check overlay properties
      (dolist (overlay alloc-scan--overlays)
        (message "  Overlay: %d-%d face:%s text:%s"
                 (overlay-start overlay)
                 (overlay-end overlay)
                 (overlay-get overlay 'face)
                 (overlay-get overlay 'after-string))))))

;;; Main test runner

(defun run-alloc-scan-tests ()
  "Run all alloc-scan tests."
  (interactive)
  (message "Running alloc-scan tests...")
  (test-basic-regex)
  (test-regex-on-content)
  (test-file-reading)
  (test-parse-dump-file)
  (test-file-matching)
  (test-with-minimal-dump)
  (test-highlighting-logic)
  (message "\n=== All Tests Complete ==="))

;; Auto-run tests when loading this file
(run-alloc-scan-tests)

(provide 'test-alloc-scan)

;;; test-alloc-scan.el ends here