;;; test-cmm-decoding.el --- Tests for CMM allocation decoding -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the CMM allocation number decoding functionality
;; based on the research about OCaml's allocation encoding.

;;; Code:

(load-file (expand-file-name "../alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

;;; Test: alloc-scan--decode-allocation-number

(defun test-decode-tuple-allocation ()
  "Test decoding tuple allocation (2048 = (2 << 10) + 0).
Expected: (2 . 0) - 2 words, tag 0"
  (let ((input 2048)
        (expected '(2 . 0)))
    (let ((result (alloc-scan--decode-allocation-number input)))
      (if (equal result expected)
          (message "✓ PASS: decode-tuple-allocation")
        (message "✗ FAIL: decode-tuple-allocation - expected %S, got %S" expected result))
      result)))

(defun test-decode-string-allocation ()
  "Test decoding string allocation with tag 252.
Expected: size in words, tag 252"
  (let ((input (+ (* 5 1024) 252))  ; 5 words, string tag
        (expected '(5 . 252)))
    (let ((result (alloc-scan--decode-allocation-number input)))
      (if (equal result expected)
          (message "✓ PASS: decode-string-allocation")
        (message "✗ FAIL: decode-string-allocation - expected %S, got %S" expected result))
      result)))

(defun test-decode-closure-allocation ()
  "Test decoding closure allocation with tag 247.
Expected: size in words, tag 247"
  (let ((input (+ (* 3 1024) 247))  ; 3 words, closure tag
        (expected '(3 . 247)))
    (let ((result (alloc-scan--decode-allocation-number input)))
      (if (equal result expected)
          (message "✓ PASS: decode-closure-allocation")
        (message "✗ FAIL: decode-closure-allocation - expected %S, got %S" expected result))
      result)))

(defun test-decode-variant-allocation ()
  "Test decoding variant allocation with tag 42.
Expected: size in words, tag 42 (variant)"
  (let ((input (+ (* 4 1024) 42))   ; 4 words, variant tag 42
        (expected '(4 . 42)))
    (let ((result (alloc-scan--decode-allocation-number input)))
      (if (equal result expected)
          (message "✓ PASS: decode-variant-allocation")
        (message "✗ FAIL: decode-variant-allocation - expected %S, got %S" expected result))
      result)))

(defun test-decode-invalid-allocation ()
  "Test decoding invalid allocation numbers.
Expected: nil for invalid inputs"
  (let ((inputs '(-1 nil "invalid"))
        (expected nil))
    (dolist (input inputs)
      (let ((result (alloc-scan--decode-allocation-number input)))
        (if (equal result expected)
            (message "✓ PASS: decode-invalid-allocation (%S)" input)
          (message "✗ FAIL: decode-invalid-allocation (%S) - expected %S, got %S" input expected result))))))

;;; Test: alloc-scan--allocation-type-name

(defun test-allocation-type-names ()
  "Test allocation type name mapping.
Expected: Human-readable names for various tags"
  (let ((test-cases '((0 "tuple/record")
                      (42 "variant-42") 
                      (246 "lazy")
                      (247 "closure")
                      (248 "object")
                      (252 "string")
                      (253 "float")
                      (254 "float-array")
                      (255 "custom")
                      (999 "tag-999"))))
    (dolist (case test-cases)
      (let* ((tag (car case))
             (expected (cadr case))
             (result (alloc-scan--allocation-type-name tag)))
        (if (equal result expected)
            (message "✓ PASS: allocation-type-name tag %d" tag)
          (message "✗ FAIL: allocation-type-name tag %d - expected %S, got %S" tag expected result))))))

;;; Test: alloc-scan--allocation-memory-info

(defun test-memory-info-calculation ()
  "Test memory info calculation.
Expected: Total words includes header for all types, bytes calculated correctly"
  (let ((test-cases '((2 (3 . 24))     ; 2 data + 1 header = 3 words, 24 bytes
                      (1 (2 . 16))     ; 1 data + 1 header = 2 words, 16 bytes  
                      (10 (11 . 88))))) ; 10 data + 1 header = 11 words, 88 bytes
    (dolist (case test-cases)
      (let* ((size-words (nth 0 case))
             (expected (nth 1 case))
             (result (alloc-scan--allocation-memory-info size-words)))
        (if (= (car result) (car expected))
            (message "✓ PASS: memory-info-calculation %d words" size-words)
          (message "✗ FAIL: memory-info-calculation %d words - expected %S words, got %S" 
                   size-words (car expected) (car result)))))))

;;; Test: alloc-scan--format-allocation-info

(defun test-format-allocation-info ()
  "Test allocation info formatting.
Expected: Human-readable formatted strings"
  (let ((test-cases '((2048 "tuple/record: 2 words")     ; (2 << 10) + 0
                      (3319 "closure: 3 words")         ; (3 << 10) + 247
                      (1024 "tuple/record: 1 words"))))  ; (1 << 10) + 0
    (dolist (case test-cases)
      (let* ((alloc-number (car case))
             (expected-contains (cadr case))
             (result (alloc-scan--format-allocation-info alloc-number)))
        (if (string-match-p (regexp-quote expected-contains) result)
            (message "✓ PASS: format-allocation-info %d" alloc-number)
          (message "✗ FAIL: format-allocation-info %d - expected to contain %S, got %S" 
                   alloc-number expected-contains result))))))

;;; Test: Sample file decoding

(defun test-sample-file-decoding ()
  "Test decoding allocations from sample file.
Expected: All allocations should decode properly"
  (let ((sample-file (expand-file-name "sample.cmx.dump" 
                                       (file-name-directory (or load-file-name buffer-file-name)))))
    (when (file-readable-p sample-file)
      (let ((allocations (alloc-scan--parse-dump-file sample-file))
            (decoded-count 0)
            (total-count 0))
        (dolist (alloc allocations)
          (setq total-count (1+ total-count))
          (let ((alloc-number (nth 4 alloc)))
            (when (alloc-scan--decode-allocation-number alloc-number)
              (setq decoded-count (1+ decoded-count)))))
        
        (if (= decoded-count total-count)
            (message "✓ PASS: sample-file-decoding - %d/%d allocations decoded" decoded-count total-count)
          (message "✗ FAIL: sample-file-decoding - only %d/%d allocations decoded" decoded-count total-count))))))

;;; Integration test: Statistics with decoding

(defun test-statistics-with-decoding ()
  "Test statistics generation with CMM decoding.
Expected: Statistics should include decoded information"
  (let ((test-allocations '(("test1.ml" 10 5 15 2048)   ; 2-word tuple
                            ("test2.ml" 20 10 25 3319)  ; 3-word closure
                            ("test1.ml" 30 0 10 1024)))) ; 1-word tuple
    (let ((stats (alloc-scan--analyze-allocations test-allocations)))
      (let ((total-words (alist-get 'total-words stats))
            (alloc-types (alist-get 'allocation-types stats)))
        (if (and (= total-words 6)  ; 2 + 3 + 1 = 6 words
                 alloc-types
                 (> (length alloc-types) 0))
            (message "✓ PASS: statistics-with-decoding")
          (message "✗ FAIL: statistics-with-decoding - total-words: %S, types: %S" 
                   total-words alloc-types))))))

;;; Test runner

(defun run-cmm-decoding-tests ()
  "Run all CMM decoding tests."
  (interactive)
  (message "\n=== Running CMM Decoding Tests ===\n")
  
  ;; Basic decoding tests
  (test-decode-tuple-allocation)
  (test-decode-string-allocation)
  (test-decode-closure-allocation)
  (test-decode-variant-allocation)
  (test-decode-invalid-allocation)
  
  ;; Type name tests
  (test-allocation-type-names)
  
  ;; Memory calculation tests
  (test-memory-info-calculation)
  
  ;; Formatting tests
  (test-format-allocation-info)
  
  ;; Integration tests
  (test-sample-file-decoding)
  (test-statistics-with-decoding)
  
  (message "\n=== CMM Decoding Tests Complete ==="))

;; Auto-run tests when loading this file
(run-cmm-decoding-tests)

(provide 'test-cmm-decoding)

;;; test-cmm-decoding.el ends here