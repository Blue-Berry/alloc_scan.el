;;; baseline-test.el --- Baseline test for alloc-scan refactoring -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple baseline test to ensure refactoring doesn't break functionality

;;; Code:

(load-file (expand-file-name "../alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

(setq alloc-scan-debug nil)

(defun baseline-test ()
  "Run baseline test with known good data."
  (let ((sample-file (expand-file-name "sample.cmx.dump" (file-name-directory (or load-file-name buffer-file-name))))
        (otree-file (expand-file-name "otree-sample.cmx.dump" (file-name-directory (or load-file-name buffer-file-name)))))
    
    (message "=== Baseline Test ===")
    
    ;; Test sample.cmx.dump (should find 6 allocations)
    (if (file-readable-p sample-file)
        (let ((allocations (alloc-scan--parse-dump-file sample-file)))
          (message "Sample file: %d allocations (expected: 6)" (length allocations))
          (if (= (length allocations) 6)
              (message "✓ Sample file test PASSED")
            (message "✗ Sample file test FAILED - expected 6, got %d" (length allocations))))
      (message "✗ Sample file not readable"))
    
    ;; Test otree-sample.cmx.dump (multi-location allocations)
    (if (file-readable-p otree-file)
        (let ((allocations (alloc-scan--parse-dump-file otree-file)))
          (message "Otree file: %d allocations" (length allocations))
          (if (> (length allocations) 0)
              (message "✓ Otree file test PASSED")
            (message "✗ Otree file test FAILED - no allocations found")))
      (message "✗ Otree file not readable"))
    
    (message "=== Baseline Test Complete ===")))

;; Run the test
(baseline-test)

(provide 'baseline-test)

;;; baseline-test.el ends here