;;; unit-tests.el --- Unit tests for individual alloc-scan functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Focused unit tests for individual functions in alloc-scan.el
;; Each test has a clear expected output and tests one specific function.

;;; Code:

(load-file (expand-file-name "../alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

;;; Test: alloc-scan--parse-curly-brace-allocations

(defun test-parse-single-curly-allocation ()
  "Test parsing a single curly brace allocation.
Expected: List with one allocation: (\"command.ml\" 100 5 15 1024)"
  (let ((input "      (alloc{command.ml:100,5-15} 1024")
        (expected '(("command.ml" 100 5 15 1024))))
    (let ((result (alloc-scan--parse-curly-brace-allocations input)))
      (if (equal result expected)
          (message "✓ PASS: parse-single-curly-allocation")
        (message "✗ FAIL: parse-single-curly-allocation - expected %S, got %S" expected result))
      result)))

(defun test-parse-empty-content ()
  "Test parsing empty content.
Expected: Empty list ()"
  (let ((input "")
        (expected '()))
    (let ((result (alloc-scan--parse-curly-brace-allocations input)))
      (if (equal result expected)
          (message "✓ PASS: parse-empty-content")
        (message "✗ FAIL: parse-empty-content - expected %S, got %S" expected result))
      result)))

(defun test-parse-no-allocations ()
  "Test parsing content with no allocations.
Expected: Empty list ()"
  (let ((input "some random text without allocations")
        (expected '()))
    (let ((result (alloc-scan--parse-curly-brace-allocations input)))
      (if (equal result expected)
          (message "✓ PASS: parse-no-allocations")
        (message "✗ FAIL: parse-no-allocations - expected %S, got %S" expected result))
      result)))

;;; Test: alloc-scan--parse-description-allocations

(defun test-parse-description-allocation ()
  "Test parsing description format allocation.
Expected: List with one allocation: (\"test.ml\" 50 10 20 2048)"
  (let ((input "(alloc 2048 \"some description [test.ml:50,10--20] info\")")
        (expected '(("test.ml" 50 10 20 2048))))
    (let ((result (alloc-scan--parse-description-allocations input)))
      (if (equal result expected)
          (message "✓ PASS: parse-description-allocation")
        (message "✗ FAIL: parse-description-allocation - expected %S, got %S" expected result))
      result)))

;;; Test: alloc-scan--filter-allocations-for-file

(defun test-filter-by-exact-path ()
  "Test filtering allocations by exact file path.
Expected: List with one matching allocation"
  (let ((allocations '(("src/main.ml" 10 5 15 1024)
                       ("lib/utils.ml" 20 10 25 2048)
                       ("test/test.ml" 30 0 10 512)))
        (current-file "/project/src/main.ml")
        (expected '(("src/main.ml" 10 5 15 1024))))
    (let ((result (alloc-scan--filter-allocations-for-file allocations current-file)))
      (if (equal result expected)
          (message "✓ PASS: filter-by-exact-path")
        (message "✗ FAIL: filter-by-exact-path - expected %S, got %S" expected result))
      result)))

(defun test-filter-by-basename ()
  "Test filtering allocations by file basename.
Expected: List with one matching allocation"
  (let ((allocations '(("different/path/main.ml" 10 5 15 1024)
                       ("another/dir/other.ml" 20 10 25 2048)))
        (current-file "/tmp/main.ml")
        (expected '(("different/path/main.ml" 10 5 15 1024))))
    (let ((result (alloc-scan--filter-allocations-for-file allocations current-file)))
      (if (equal result expected)
          (message "✓ PASS: filter-by-basename")
        (message "✗ FAIL: filter-by-basename - expected %S, got %S" expected result))
      result)))

(defun test-filter-no-matches ()
  "Test filtering when no files match.
Expected: Empty list ()"
  (let ((allocations '(("src/main.ml" 10 5 15 1024)
                       ("lib/utils.ml" 20 10 25 2048)))
        (current-file "/tmp/nomatch.ml")
        (expected '()))
    (let ((result (alloc-scan--filter-allocations-for-file allocations current-file)))
      (if (equal result expected)
          (message "✓ PASS: filter-no-matches")
        (message "✗ FAIL: filter-no-matches - expected %S, got %S" expected result))
      result)))

;;; Test: alloc-scan--calculate-positions

(defun test-calculate-positions-valid ()
  "Test calculating positions for valid line and columns.
Expected: Cons cell with start < end positions"
  (with-temp-buffer
    (insert "line 1\n")
    (insert "line 2 with text\n")
    (insert "line 3\n")
    (let ((result (alloc-scan--calculate-positions 2 5 10)))
      (if (and result 
               (consp result) 
               (< (car result) (cdr result))
               (numberp (car result))
               (numberp (cdr result)))
          (message "✓ PASS: calculate-positions-valid - got (%d . %d)" (car result) (cdr result))
        (message "✗ FAIL: calculate-positions-valid - expected valid cons, got %S" result))
      result)))

(defun test-calculate-positions-invalid-line ()
  "Test calculating positions for invalid line number.
Expected: nil"
  (with-temp-buffer
    (insert "only one line\n")
    (let ((result (alloc-scan--calculate-positions 5 0 10)))
      (if (null result)
          (message "✓ PASS: calculate-positions-invalid-line")
        (message "✗ FAIL: calculate-positions-invalid-line - expected nil, got %S" result))
      result)))

(defun test-calculate-positions-invalid-column ()
  "Test calculating positions for columns beyond line length.
Expected: nil"
  (with-temp-buffer
    (insert "short\n")
    (let ((result (alloc-scan--calculate-positions 1 50 60)))
      (if (null result)
          (message "✓ PASS: calculate-positions-invalid-column")
        (message "✗ FAIL: calculate-positions-invalid-column - expected nil, got %S" result))
      result)))

;;; Test: alloc-scan--analyze-allocations

(defun test-analyze-allocations-basic ()
  "Test basic allocation analysis.
Expected: Statistics with total-allocations=2, files-with-allocations=2, total-words=3"
  (let ((allocations '(("file1.ml" 10 0 10 1024)   ; 1 word tuple
                       ("file2.ml" 20 0 15 2048)))  ; 2 word tuple
        (expected-total 2)
        (expected-files 2)
        (expected-words 3))  ; 1 + 2 = 3 words
    (let ((result (alloc-scan--analyze-allocations allocations)))
      (if (and (= (alist-get 'total-allocations result) expected-total)
               (= (alist-get 'files-with-allocations result) expected-files)
               (= (alist-get 'total-words result) expected-words))
          (message "✓ PASS: analyze-allocations-basic")
        (message "✗ FAIL: analyze-allocations-basic - got %S" result))
      result)))

(defun test-analyze-empty-allocations ()
  "Test analyzing empty allocation list.
Expected: nil"
  (let ((allocations '()))
    (let ((result (alloc-scan--analyze-allocations allocations)))
      (if (null result)
          (message "✓ PASS: analyze-empty-allocations")
        (message "✗ FAIL: analyze-empty-allocations - expected nil, got %S" result))
      result)))

;;; Test: alloc-scan--get-highlight-face

(defun test-get-highlight-face-box ()
  "Test getting box highlight face.
Expected: Face spec with :box property"
  (let ((alloc-scan-highlight-style 'box)
        (alloc-scan-highlight-color "#ff0000"))
    (let ((result (alloc-scan--get-highlight-face)))
      (if (and (listp result) (plist-get result :box))
          (message "✓ PASS: get-highlight-face-box")
        (message "✗ FAIL: get-highlight-face-box - expected face with :box, got %S" result))
      result)))

(defun test-get-highlight-face-underline ()
  "Test getting underline highlight face.
Expected: Face spec with :underline property"
  (let ((alloc-scan-highlight-style 'underline)
        (alloc-scan-highlight-color "#00ff00"))
    (let ((result (alloc-scan--get-highlight-face)))
      (if (and (listp result) (plist-get result :underline))
          (message "✓ PASS: get-highlight-face-underline")
        (message "✗ FAIL: get-highlight-face-underline - expected face with :underline, got %S" result))
      result)))

;;; Test: alloc-scan--cache-key

(defun test-cache-key-existing-file ()
  "Test cache key generation for existing file.
Expected: Cons cell with file path and modification time"
  (let ((temp-file (make-temp-file "alloc-scan-test")))
    (unwind-protect
        (let ((result (alloc-scan--cache-key temp-file)))
          (if (and result 
                   (consp result)
                   (string= (car result) temp-file)
                   (cdr result))
              (message "✓ PASS: cache-key-existing-file")
            (message "✗ FAIL: cache-key-existing-file - expected valid cache key, got %S" result))
          result)
      (delete-file temp-file))))

(defun test-cache-key-nonexistent-file ()
  "Test cache key generation for non-existent file.
Expected: nil"
  (let ((result (alloc-scan--cache-key "/non/existent/file")))
    (if (null result)
        (message "✓ PASS: cache-key-nonexistent-file")
      (message "✗ FAIL: cache-key-nonexistent-file - expected nil, got %S" result))
    result))

;;; Test runner

(defun run-unit-tests ()
  "Run all unit tests."
  (interactive)
  (message "\n=== Running Unit Tests ===\n")
  
  ;; Parsing tests
  (test-parse-single-curly-allocation)
  (test-parse-empty-content)
  (test-parse-no-allocations)
  (test-parse-description-allocation)
  
  ;; Filtering tests
  (test-filter-by-exact-path)
  (test-filter-by-basename)
  (test-filter-no-matches)
  
  ;; Position calculation tests
  (test-calculate-positions-valid)
  (test-calculate-positions-invalid-line)
  (test-calculate-positions-invalid-column)
  
  ;; Analysis tests
  (test-analyze-allocations-basic)
  (test-analyze-empty-allocations)
  
  ;; Face tests
  (test-get-highlight-face-box)
  (test-get-highlight-face-underline)
  
  ;; Cache tests
  (test-cache-key-existing-file)
  (test-cache-key-nonexistent-file)
  
  (message "\n=== Unit Tests Complete ==="))

;; Auto-run tests when loading this file
(run-unit-tests)

(provide 'unit-tests)

;;; unit-tests.el ends here