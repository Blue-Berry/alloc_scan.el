;;; debug-highlight.el --- Debug highlighting issue -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(message "=== DEBUGGING HIGHLIGHT ISSUE ===")

;; Test data
(let ((test-allocations '(("command/src/command.ml" 2583 16 40 3319)
                         ("command/src/command.ml" 2592 16 43 3319)
                         ("command/src/command.ml" 2594 6 15 1024)))
      (test-file "/tmp/command.ml"))
  
  ;; Create test file with enough lines
  (with-temp-file test-file
    (dotimes (i 2600)
      (insert (format "Line %d - some content here for testing\n" (1+ i)))))
  
  ;; Test highlighting
  (with-temp-buffer
    (insert-file-contents test-file)
    (setq buffer-file-name test-file)
    
    (message "Buffer file name: %s" buffer-file-name)
    (message "Buffer has %d lines" (line-number-at-pos (point-max)))
    
    ;; Test each allocation manually
    (message "\nTesting individual allocations:")
    (dolist (alloc test-allocations)
      (let ((filepath (nth 0 alloc))
            (line (nth 1 alloc))
            (col-start (nth 2 alloc))
            (col-end (nth 3 alloc))
            (blocks (nth 4 alloc)))
        
        (message "Testing allocation: %s:%d,%d-%d" filepath line col-start col-end)
        
        ;; Test file matching
        (let ((suffix-match (string-suffix-p filepath buffer-file-name))
              (basename-match (string-suffix-p (file-name-nondirectory filepath)
                                             (file-name-nondirectory buffer-file-name))))
          (message "  File match - suffix:%s basename:%s" suffix-match basename-match)
          
          (when (or suffix-match basename-match)
            (message "  File matches! Attempting to create overlay...")
            
            ;; Check if line exists
            (save-excursion
              (goto-char (point-min))
              (if (> line (line-number-at-pos (point-max)))
                  (message "  ERROR: Line %d doesn't exist (max: %d)" line (line-number-at-pos (point-max)))
                (progn
                  (forward-line (1- line))
                  (let ((line-start (point))
                        (line-end (line-end-position))
                        (line-length (- (line-end-position) (point))))
                    (message "  Line %d: length=%d, col-start=%d, col-end=%d" line line-length col-start col-end)
                    (message "  Line content: '%s'" (buffer-substring line-start line-end))
                    
                    ;; Check column bounds
                    (if (> col-start line-length)
                        (message "  ERROR: col-start %d > line-length %d" col-start line-length)
                      (message "  Column bounds OK"))
                    
                    ;; Try to create overlay
                    (let ((overlay (alloc-scan--create-overlay line col-start col-end blocks)))
                      (if overlay
                          (message "  SUCCESS: Created overlay %s" overlay)
                        (message "  ERROR: Failed to create overlay"))))))))))
    
    ;; Test the full highlighting function
    (message "\nTesting full highlighting function:")
    (alloc-scan--highlight-allocations test-allocations)
    (message "Created %d overlays total" (length alloc-scan--overlays))
    (dolist (overlay alloc-scan--overlays)
      (message "  Overlay: %d-%d, text: %s" 
               (overlay-start overlay)
               (overlay-end overlay)
               (overlay-get overlay 'after-string))))
  
  ;; Clean up
  (delete-file test-file))