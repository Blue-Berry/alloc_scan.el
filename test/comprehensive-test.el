;;; comprehensive-test.el --- Complete plugin tests -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(defvar test-results '())

(defun test-log (test-name status message)
  "Log test result."
  (let ((result (list test-name status message)))
    (push result test-results)
    (message "%s: %s - %s" test-name (if status "PASS" "FAIL") message)))

(defun test-parsing ()
  "Test the parsing function."
  (message "\n=== TESTING PARSING FUNCTION ===")
  (let ((dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
    
    ;; Test 1: File exists and is readable
    (test-log "File Readable" 
              (file-readable-p dump-file)
              (format "Dump file: %s" dump-file))
    
    ;; Test 2: Parsing returns results
    (let ((result (alloc-scan--parse-dump-file dump-file)))
      (test-log "Parsing Returns Data"
                (> (length result) 0)
                (format "Found %d allocations" (length result)))
      
      ;; Test 3: Expected number of allocations
      (test-log "Expected Count"
                (= (length result) 6)
                (format "Expected 6, got %d" (length result)))
      
      ;; Test 4: Data structure is correct
      (when result
        (let ((first-alloc (car result)))
          (test-log "Data Structure"
                    (and (listp first-alloc) (= (length first-alloc) 5))
                    (format "First allocation: %s" first-alloc))))
      
      result)))

(defun test-file-matching ()
  "Test file matching logic."
  (message "\n=== TESTING FILE MATCHING ===")
  (let ((test-allocations '(("command/src/command.ml" 2583 16 40 3319)
                           ("lib/test.ml" 100 5 20 1024))))
    
    ;; Test matching with exact path
    (let ((current-file "/home/user/project/command/src/command.ml"))
      (with-temp-buffer
        (setq buffer-file-name current-file)
        (let ((matches 0))
          (dolist (alloc test-allocations)
            (let ((filepath (nth 0 alloc)))
              (when (or (string-suffix-p filepath current-file)
                       (string-suffix-p (file-name-nondirectory filepath)
                                      (file-name-nondirectory current-file)))
                (setq matches (1+ matches)))))
          (test-log "Exact Path Match"
                    (= matches 1)
                    (format "Expected 1 match, got %d" matches)))))
    
    ;; Test matching with basename only
    (let ((current-file "/tmp/command.ml"))
      (with-temp-buffer
        (setq buffer-file-name current-file)
        (let ((matches 0))
          (dolist (alloc test-allocations)
            (let ((filepath (nth 0 alloc)))
              (when (or (string-suffix-p filepath current-file)
                       (string-suffix-p (file-name-nondirectory filepath)
                                      (file-name-nondirectory current-file)))
                (setq matches (1+ matches)))))
          (test-log "Basename Match"
                    (= matches 1)
                    (format "Expected 1 match, got %d" matches)))))))

(defun test-overlay-creation ()
  "Test overlay creation."
  (message "\n=== TESTING OVERLAY CREATION ===")
  (with-temp-buffer
    (insert "line 1\n")
    (insert "line 2\n") 
    (insert "line 3 - this should be highlighted\n")
    (insert "line 4\n")
    
    ;; Test creating an overlay
    (let ((overlay (alloc-scan--create-overlay 3 10 20 1024)))
      (test-log "Overlay Creation"
                (overlayp overlay)
                "Created overlay successfully")
      
      ;; Test overlay properties
      (test-log "Overlay Face"
                (eq (overlay-get overlay 'face) alloc-scan-highlight-face)
                "Face property set correctly")
      
      ;; Test virtual text
      (let ((after-string (overlay-get overlay 'after-string)))
        (test-log "Virtual Text"
                  (and after-string (string-match "Blocks:" after-string))
                  (format "After-string: %s" after-string)))
      
      ;; Clean up
      (delete-overlay overlay))))

(defun test-integration ()
  "Test full integration with real file."
  (message "\n=== TESTING INTEGRATION ===")
  
  ;; Create a test file matching the allocations
  (let ((test-file "/tmp/command.ml")
        (dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
    
    ;; Create test file
    (with-temp-file test-file
      (insert "// Line 1\n")
      (insert "// Line 2\n")
      (dotimes (i 2590)  ; Create enough lines to reach line 2583
        (insert (format "// Line %d\n" (+ i 3)))))
    
    ;; Test with the file
    (with-temp-buffer
      (insert-file-contents test-file)
      (setq buffer-file-name test-file)
      
      ;; Parse allocations
      (let ((allocations (alloc-scan--parse-dump-file dump-file)))
        (test-log "Integration Parse"
                  (> (length allocations) 0)
                  (format "Parsed %d allocations" (length allocations)))
        
        ;; Test highlighting
        (alloc-scan--highlight-allocations allocations)
        (test-log "Integration Highlight"
                  (> (length alloc-scan--overlays) 0)
                  (format "Created %d overlays" (length alloc-scan--overlays)))
        
        ;; Test clearing
        (alloc-scan--clear-overlays)
        (test-log "Integration Clear"
                  (= (length alloc-scan--overlays) 0)
                  "Cleared all overlays")))
    
    ;; Clean up test file
    (delete-file test-file)))

(defun run-all-tests ()
  "Run all tests and report results."
  (message "=== RUNNING COMPREHENSIVE TESTS ===")
  (setq test-results '())
  
  ;; Run all test suites
  (test-parsing)
  (test-file-matching)
  (test-overlay-creation)
  (test-integration)
  
  ;; Report summary
  (message "\n=== TEST SUMMARY ===")
  (let ((total (length test-results))
        (passed (length (cl-remove-if-not (lambda (r) (nth 1 r)) test-results)))
        (failed (length (cl-remove-if (lambda (r) (nth 1 r)) test-results))))
    (message "Total tests: %d" total)
    (message "Passed: %d" passed)
    (message "Failed: %d" failed)
    (message "Success rate: %.1f%%" (* 100.0 (/ (float passed) total)))
    
    ;; Show failed tests
    (when (> failed 0)
      (message "\nFAILED TESTS:")
      (dolist (result test-results)
        (unless (nth 1 result)
          (message "  %s: %s" (nth 0 result) (nth 2 result)))))))

;; Run tests
(run-all-tests)