;;; test-auto-refresh.el --- Test auto-refresh functionality

;; Create a test to verify auto-refresh works
(load-file (expand-file-name "alloc-scan.el" (file-name-directory (or load-file-name buffer-file-name))))

(setq alloc-scan-debug t)
(setq alloc-scan-polling-interval 1) ; Fast polling for testing

(defun test-auto-refresh ()
  "Test auto-refresh by modifying a test file."
  (let* ((test-file "/tmp/test-dump.cmx")
         (test-content-1 "some content\n(alloc{test.ml:10,5-15} 1024)\nmore content")
         (test-content-2 "some content\n(alloc{test.ml:10,5-15} 1024)\n(alloc{test.ml:20,10-20} 2048)\nmore content"))
    
    (message "=== Auto-refresh Test ===")
    
    ;; Create initial test file
    (with-temp-file test-file
      (insert test-content-1))
    
    ;; Parse and start watching
    (let ((allocations-1 (alloc-scan--parse-dump-file test-file)))
      (message "Initial parse: %d allocations" (length allocations-1))
      
      ;; Start watching in a temp buffer
      (with-temp-buffer
        (setq buffer-file-name "/tmp/test.ml")
        (alloc-scan--highlight-allocations allocations-1 test-file)
        
        (message "Started watching %s" test-file)
        (message "Sleeping 2 seconds...")
        (sleep-for 2)
        
        ;; Modify the file
        (message "Modifying test file...")
        (with-temp-file test-file
          (insert test-content-2))
        
        (message "File modified, waiting for auto-refresh...")
        (sleep-for 3) ; Wait for polling to detect change
        
        (message "Auto-refresh test completed")))
    
    ;; Cleanup
    (when (file-exists-p test-file)
      (delete-file test-file))))

;; Run the test
(test-auto-refresh)

;;; test-auto-refresh.el ends here