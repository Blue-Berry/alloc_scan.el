;;; test-alloc-scan-suite.el --- Comprehensive test suite for alloc-scan -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive test suite for alloc-scan.el with clear test names,
;; expected outputs, and structured assertions.

;;; Code:

(load-file (expand-file-name "../alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

;; Test configuration
(setq alloc-scan-debug nil)

;; Test framework utilities
(defvar test-results '()
  "List to store test results.")

(defvar test-sample-dump-file 
  (expand-file-name "sample.cmx.dump" (file-name-directory (or load-file-name buffer-file-name)))
  "Path to sample dump file for testing.")

(defmacro deftest (name expected-description &rest body)
  "Define a test with NAME and EXPECTED-DESCRIPTION, executing BODY."
  `(defun ,(intern (concat "test-" (symbol-name name))) ()
     ,(format "Test: %s\nExpected: %s" name expected-description)
     (condition-case err
         (let ((result (progn ,@body)))
           (push (list ',name t result ,expected-description) test-results)
           (message "✓ PASS: %s" ',name)
           result)
       (error
        (push (list ',name nil (error-message-string err) ,expected-description) test-results)
        (message "✗ FAIL: %s - %s" ',name (error-message-string err))
        nil))))

(defun assert-equal (expected actual message)
  "Assert that EXPECTED equals ACTUAL, showing MESSAGE on failure."
  (unless (equal expected actual)
    (error "%s: expected %S, got %S" message expected actual))
  t)

(defun assert-true (value message)
  "Assert that VALUE is non-nil, showing MESSAGE on failure."
  (unless value
    (error "%s: expected non-nil, got %S" message value))
  t)

(defun assert-count (expected-count list message)
  "Assert that LIST has EXPECTED-COUNT elements, showing MESSAGE on failure."
  (let ((actual-count (length list)))
    (unless (= expected-count actual-count)
      (error "%s: expected %d items, got %d" message expected-count actual-count)))
  t)

;;; Core parsing tests

(deftest parse-curly-brace-format
  "Parse curly brace allocation format and return structured data"
  (let ((content "      (alloc{command/src/command.ml:2583,16-40} 3319"))
    (let ((result (alloc-scan--parse-curly-brace-allocations content)))
      (assert-count 1 result "Should find exactly one allocation")
      (let ((alloc (car result)))
        (assert-equal "command/src/command.ml" (nth 0 alloc) "File path")
        (assert-equal 2583 (nth 1 alloc) "Line number")
        (assert-equal 16 (nth 2 alloc) "Column start")
        (assert-equal 40 (nth 3 alloc) "Column end")
        (assert-equal 3319 (nth 4 alloc) "Block count"))
      result)))

(deftest parse-description-format
  "Parse description allocation format and return structured data"
  (let ((content "(alloc 1024 \"description with [file.ml:10,5--15] location\")"))
    (let ((result (alloc-scan--parse-description-allocations content)))
      (assert-count 1 result "Should find exactly one allocation")
      (let ((alloc (car result)))
        (assert-equal "file.ml" (nth 0 alloc) "File path")
        (assert-equal 10 (nth 1 alloc) "Line number")
        (assert-equal 5 (nth 2 alloc) "Column start")
        (assert-equal 15 (nth 3 alloc) "Column end")  
        (assert-equal 1024 (nth 4 alloc) "Block count"))
      result)))

(deftest parse-multiple-allocations
  "Parse multiple allocations from mixed content"
  (let ((content "some text
      (alloc{command/src/command.ml:2583,16-40} 3319
      more text
      (alloc{command/src/command.ml:2594,6-15} 1024
      final text"))
    (let ((result (alloc-scan--parse-curly-brace-allocations content)))
      (assert-count 2 result "Should find exactly two allocations")
      (assert-equal 2583 (nth 1 (nth 0 result)) "First allocation line")
      (assert-equal 2594 (nth 1 (nth 1 result)) "Second allocation line")
      result)))

(deftest parse-sample-dump-file
  "Parse the sample dump file and return expected allocation count"
  (if (file-readable-p test-sample-dump-file)
      (let ((result (alloc-scan--parse-dump-file test-sample-dump-file)))
        (assert-count 6 result "Sample file should contain 6 allocations")
        (assert-true (cl-every (lambda (alloc) (= (length alloc) 5)) result)
                     "Each allocation should have 5 elements")
        result)
    (error "Sample dump file not readable: %s" test-sample-dump-file)))

;;; File matching tests

(deftest filter-allocations-by-suffix
  "Filter allocations to match current file by path suffix"
  (let ((allocations '(("command/src/command.ml" 100 10 20 1024)
                       ("lib/other.ml" 200 30 40 2048)
                       ("test/example.ml" 300 50 60 512)))
        (current-file "/home/user/project/command/src/command.ml"))
    (let ((result (alloc-scan--filter-allocations-for-file allocations current-file)))
      (assert-count 1 result "Should match exactly one file")
      (assert-equal "command/src/command.ml" (nth 0 (car result)) "Should match command.ml")
      result)))

(deftest filter-allocations-by-basename
  "Filter allocations to match current file by basename"
  (let ((allocations '(("different/path/test.ml" 100 10 20 1024)
                       ("another/location/other.ml" 200 30 40 2048)))
        (current-file "/tmp/test.ml"))
    (let ((result (alloc-scan--filter-allocations-for-file allocations current-file)))
      (assert-count 1 result "Should match exactly one file by basename")
      (assert-equal "different/path/test.ml" (nth 0 (car result)) "Should match test.ml")
      result)))

;;; Position calculation tests

(deftest calculate-valid-positions
  "Calculate buffer positions for valid line and column ranges"
  (with-temp-buffer
    (insert "line 1\n")
    (insert "line 2 with some text\n")
    (insert "line 3\n")
    (let ((result (alloc-scan--calculate-positions 2 5 15)))
      (assert-true result "Should return valid positions")
      (assert-true (< (car result) (cdr result)) "Start should be less than end")
      result)))

(deftest calculate-invalid-positions
  "Return nil for invalid line and column ranges"
  (with-temp-buffer
    (insert "short line\n")
    (let ((result (alloc-scan--calculate-positions 2 50 60)))
      (assert-equal nil result "Should return nil for invalid positions")
      result)))

;;; Overlay creation tests

(deftest create-overlay-with-valid-position
  "Create overlay with proper face and virtual text"
  (with-temp-buffer
    (insert "line 1\n")
    (insert "line 2 with allocation here\n")
    (insert "line 3\n")
    (let ((overlay (alloc-scan--create-overlay 2 10 20 1024)))
      (assert-true overlay "Should create overlay")
      (assert-true (overlay-get overlay 'alloc-scan) "Should have alloc-scan property")
      (assert-true (overlay-get overlay 'face) "Should have face property")
      (when alloc-scan-show-virtual-text
        (assert-true (overlay-get overlay 'after-string) "Should have virtual text"))
      (delete-overlay overlay)
      overlay)))

(deftest create-overlay-with-invalid-position
  "Return nil when trying to create overlay at invalid position"
  (with-temp-buffer
    (insert "short\n")
    (let ((overlay (alloc-scan--create-overlay 2 50 60 1024)))
      (assert-equal nil overlay "Should return nil for invalid position")
      overlay)))

;;; Highlighting integration tests

(deftest highlight-allocations-in-buffer
  "Highlight relevant allocations in a test buffer"
  (let ((allocations '(("test.ml" 2 5 15 1024)
                       ("other.ml" 3 10 20 2048)
                       ("test.ml" 3 0 10 512))))
    (with-temp-buffer
      (insert "line 1\n")
      (insert "line 2 with some text to highlight\n")
      (insert "line 3 also highlighted\n")
      (setq buffer-file-name "/tmp/test.ml")
      
      (alloc-scan--highlight-allocations allocations)
      
      (assert-count 2 alloc-scan--overlays "Should create 2 overlays for test.ml")
      (dolist (overlay alloc-scan--overlays)
        (assert-true (overlay-get overlay 'alloc-scan) "Each overlay should be marked"))
      
      ;; Clean up
      (alloc-scan--clear-overlays)
      (length alloc-scan--overlays))))

(deftest clear-all-overlays
  "Clear all overlays from buffer"
  (with-temp-buffer
    (insert "test content\n")
    (setq buffer-file-name "/tmp/test.ml")
    
    ;; Create some overlays
    (alloc-scan--create-overlay 1 0 5 1024)
    (alloc-scan--create-overlay 1 6 10 2048)
    
    (assert-true (> (length alloc-scan--overlays) 0) "Should have overlays")
    
    (alloc-scan--clear-overlays)
    
    (assert-count 0 alloc-scan--overlays "Should have no overlays after clearing")
    (length alloc-scan--overlays)))

;;; Cache tests

(deftest cache-file-results
  "Cache and retrieve parsed file results"
  (when (file-readable-p test-sample-dump-file)
    ;; Clear cache first
    (alloc-scan--cache-clear)
    
    ;; First parse should miss cache
    (let ((result1 (alloc-scan--parse-dump-file test-sample-dump-file)))
      (assert-true result1 "Should parse file successfully")
      
      ;; Second parse should hit cache
      (let ((result2 (alloc-scan--parse-dump-file test-sample-dump-file)))
        (assert-equal result1 result2 "Cached result should match original")
        (assert-count (length result1) result2 "Should have same number of allocations")
        result2))))

;;; Statistics tests

(deftest analyze-allocation-statistics
  "Analyze allocation data and return statistics"
  (let ((allocations '(("file1.ml" 10 0 10 1024)
                       ("file1.ml" 20 0 15 2048)
                       ("file2.ml" 30 0 20 512))))
    (let ((stats (alloc-scan--analyze-allocations allocations)))
      (assert-equal 3 (alist-get 'total-allocations stats) "Total count")
      (assert-equal 3584 (alist-get 'total-blocks stats) "Total blocks")
      (assert-equal 2 (alist-get 'files-with-allocations stats) "File count")
      (assert-true (alist-get 'size-distribution stats) "Should have size distribution")
      stats)))

;;; Error handling tests

(deftest handle-invalid-dump-file
  "Handle parsing of non-existent or invalid dump files gracefully"
  (let ((result (alloc-scan--parse-dump-file "/non/existent/file.dump")))
    (assert-equal nil result "Should return nil for non-existent file")
    result))

(deftest handle-malformed-allocation-data
  "Handle malformed allocation data without crashing"
  (let ((content "invalid (alloc{malformed} content"))
    (let ((result (alloc-scan--parse-curly-brace-allocations content)))
      (assert-equal '() result "Should return empty list for malformed data")
      result)))

;;; Test runner

(defun run-test-suite ()
  "Run the complete alloc-scan test suite."
  (interactive)
  (message "\n=== Running Alloc-Scan Test Suite ===\n")
  (setq test-results '())
  
  ;; Run all tests
  (test-parse-curly-brace-format)
  (test-parse-description-format)
  (test-parse-multiple-allocations)
  (test-parse-sample-dump-file)
  (test-filter-allocations-by-suffix)
  (test-filter-allocations-by-basename)
  (test-calculate-valid-positions)
  (test-calculate-invalid-positions)
  (test-create-overlay-with-valid-position)
  (test-create-overlay-with-invalid-position)
  (test-highlight-allocations-in-buffer)
  (test-clear-all-overlays)
  (test-cache-file-results)
  (test-analyze-allocation-statistics)
  (test-handle-invalid-dump-file)
  (test-handle-malformed-allocation-data)
  
  ;; Report results
  (report-test-results))

(defun report-test-results ()
  "Report the test results summary."
  (let* ((total (length test-results))
         (passed (length (cl-remove-if-not #'cadr test-results)))
         (failed (- total passed))
         (success-rate (if (> total 0) (* 100.0 (/ (float passed) total)) 0)))
    
    (message "\n=== Test Results Summary ===")
    (message "Total tests: %d" total)
    (message "Passed: %d" passed)
    (message "Failed: %d" failed)
    (message "Success rate: %.1f%%\n" success-rate)
    
    ;; Show failed tests
    (when (> failed 0)
      (message "Failed tests:")
      (dolist (result test-results)
        (unless (cadr result)
          (message "  ✗ %s: %s" (car result) (caddr result))))
      (message ""))
    
    ;; Return success status
    (= failed 0)))

;; Auto-run tests when loading this file
(run-test-suite)

(provide 'test-alloc-scan-suite)

;;; test-alloc-scan-suite.el ends here