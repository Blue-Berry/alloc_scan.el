;;; simple-highlight-test.el --- Simple highlight test -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(message "Testing overlay creation directly...")

;; Create a simple test buffer
(with-temp-buffer
  (insert "Line 1\n")
  (insert "Line 2\n") 
  (insert "Line 3 with some content to highlight\n")
  (insert "Line 4\n")
  
  (setq buffer-file-name "/tmp/command.ml")
  (message "Buffer has %d lines" (line-number-at-pos (point-max)))
  
  ;; Test creating overlay on line 3
  (let ((overlay (alloc-scan--create-overlay 3 10 20 1024)))
    (message "Created overlay: %s" overlay)
    (when overlay
      (message "Overlay start: %d" (overlay-start overlay))
      (message "Overlay end: %d" (overlay-end overlay))
      (message "Overlay face: %s" (overlay-get overlay 'face))
      (message "After string: %s" (overlay-get overlay 'after-string))))
  
  ;; Test with a matching allocation
  (let ((test-allocations '(("command.ml" 3 10 20 1024))))
    (message "\nTesting highlight-allocations function...")
    (alloc-scan--highlight-allocations test-allocations)
    (message "Created %d overlays" (length alloc-scan--overlays))))

(message "Test complete")