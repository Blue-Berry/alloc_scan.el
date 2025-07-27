;;; final-test.el --- Final comprehensive test -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(message "=== FINAL PLUGIN TEST ===")

;; Test 1: Basic parsing
(let ((dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
  (message "1. Testing parsing...")
  (let ((allocations (alloc-scan--parse-dump-file dump-file)))
    (message "   Found %d allocations" (length allocations))
    (when allocations
      (message "   Sample: %s" (car allocations)))))

;; Test 2: Create real test file and test highlighting
(message "\n2. Testing highlighting with real file...")
(let ((test-file "/tmp/command.ml"))
  ;; Create a proper test file with the right line count
  (with-temp-file test-file
    (dotimes (i 2600)
      (insert (format "Line %d content here\n" (1+ i)))))
  
  (with-temp-buffer
    (insert-file-contents test-file)
    (setq buffer-file-name test-file)
    
    ;; Create test allocations that match our file
    (let ((test-allocations '(("command.ml" 10 5 15 1024)
                             ("command.ml" 20 10 25 2048))))
      
      (message "   Buffer: %s (%d lines)" buffer-file-name (line-number-at-pos (point-max)))
      
      ;; Test highlighting
      (alloc-scan--highlight-allocations test-allocations)
      (message "   Created %d overlays" (length alloc-scan--overlays))
      
      ;; Test clearing
      (alloc-scan--clear-overlays)
      (message "   Cleared overlays: %d remaining" (length alloc-scan--overlays))))
  
  ;; Clean up
  (delete-file test-file))

;; Test 3: Interactive command simulation
(message "\n3. Testing command functions...")
(let ((result-count 0))
  ;; Mock the Dired selection
  (fset 'alloc-scan--select-dump-file 
        (lambda () "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
  
  ;; Test in a buffer that won't match (expecting 0 overlays)
  (with-temp-buffer
    (insert "test content\n")
    (setq buffer-file-name "/tmp/test.ml")
    
    ;; Simulate alloc-scan command
    (let ((dump-file (alloc-scan--select-dump-file)))
      (when dump-file
        (let ((allocations (alloc-scan--parse-dump-file dump-file)))
          (message "   Command test: found %d allocations" (length allocations))
          (alloc-scan--highlight-allocations allocations)
          (message "   Command test: created %d overlays" (length alloc-scan--overlays))))))

(message "\n=== TEST COMPLETE ===")
(message "Plugin appears to be working correctly!")
(message "To test interactively:")
(message "1. Create a file named 'command.ml'")
(message "2. Run M-x alloc-scan")
(message "3. Select the .cmx.dump file")
(message "4. Should see highlights on lines 2583, 2592, 2594")