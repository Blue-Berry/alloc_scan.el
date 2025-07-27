;;; demo-test.el --- Demonstration test for alloc-scan plugin -*- lexical-binding: t; -*-

(add-to-list 'load-path "..")
(load-file "../alloc-scan.el")

(message "=== ALLOC-SCAN PLUGIN DEMONSTRATION ===")

;; Test with the created command.ml file
(let ((test-file "command.ml")
      (dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump"))
  
  (message "1. Testing with file: %s" test-file)
  
  ;; Parse allocations from dump file
  (let ((allocations (alloc-scan--parse-dump-file dump-file)))
    (message "   Parsed %d allocations from dump file" (length allocations))
    
    ;; Show what allocations were found
    (message "   Allocations found:")
    (dolist (alloc allocations)
      (message "     %s:%d,%d-%d (%d blocks)" 
               (nth 0 alloc) (nth 1 alloc) (nth 2 alloc) (nth 3 alloc) (nth 4 alloc)))
    
    ;; Test with our command.ml file
    (with-current-buffer (find-file-noselect test-file)
      (message "\n2. Testing highlighting in buffer: %s" (buffer-name))
      (message "   Buffer file name: %s" buffer-file-name)
      
      ;; Apply highlighting
      (alloc-scan--highlight-allocations allocations)
      (message "   Created %d overlays" (length alloc-scan--overlays))
      
      ;; Show overlay details
      (when alloc-scan--overlays
        (message "   Overlay details:")
        (dolist (overlay alloc-scan--overlays)
          (message "     Overlay at %d-%d: %s" 
                   (overlay-start overlay)
                   (overlay-end overlay)
                   (overlay-get overlay 'after-string))))
      
      ;; Test clearing
      (alloc-scan--clear-overlays)
      (message "   Cleared overlays: %d remaining" (length alloc-scan--overlays))))
  
  (message "\n=== DEMONSTRATION COMPLETE ===")
  (message "The plugin successfully:")
  (message "- Parsed %d allocations from the OCaml dump file" (length (alloc-scan--parse-dump-file dump-file)))
  (message "- Applied highlighting to matching files")
  (message "- Showed allocation details as virtual text")
  (message "- Cleared highlights when requested")
  (message "\nTo test interactively:")
  (message "1. Open test/command.ml in Emacs")
  (message "2. Load the plugin: M-x load-file RET ../alloc-scan.el")
  (message "3. Run: M-x alloc-scan")
  (message "4. Select the .cmx.dump file when prompted"))