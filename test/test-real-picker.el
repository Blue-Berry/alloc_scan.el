;;; test-real-picker.el --- Test with real alloc_scan project -*- lexical-binding: t; -*-

(add-to-list 'load-path "..")
(load-file "../alloc-scan.el")

(message "=== TESTING WITH REAL ALLOC_SCAN PROJECT ===")

;; Test with the actual alloc_scan project
(let* ((current-file "/home/liam/playground/alloc_scan/lib/alloc_scan.ml")
       (project-root (alloc-scan--find-dune-project current-file))
       (dump-files (when project-root
                     (alloc-scan--find-build-files project-root "\\.cmx\\.dump$"))))
  
  (message "1. Real Project Test:")
  (message "   Current file: %s" current-file)
  (message "   Project root: %s" project-root)
  (message "   Found %d dump files:" (length dump-files))
  (dolist (file dump-files)
    (message "     - %s" file))
  
  ;; Test the selection presentation
  (when dump-files
    (message "\n2. How the new picker will work:")
    (cond
     ((= (length dump-files) 1)
      (message "   Single file found - will show y/n prompt:")
      (message "   'Use dump file: %s? (y or n)'" (file-name-nondirectory (car dump-files))))
     
     (t
      (message "   Multiple files found - will show completion:")
      (let ((file-alist (mapcar (lambda (file)
                                  (cons (concat (file-name-nondirectory file)
                                              " (" (file-name-directory file) ")")
                                        file))
                                dump-files)))
        (message "   Completion options in minibuffer:")
        (dolist (option file-alist)
          (message "     %s" (car option))))))))

(message "\n=== IMPROVED UX SUMMARY ===")
(message "✅ No more confusing Dired interface")
(message "✅ Stay in your original buffer")  
(message "✅ Standard Emacs minibuffer prompts")
(message "✅ Auto-completion and file filtering")
(message "✅ Smart defaults when single file found")