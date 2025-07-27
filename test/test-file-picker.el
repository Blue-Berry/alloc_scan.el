;;; test-file-picker.el --- Test the new file picker -*- lexical-binding: t; -*-

(add-to-list 'load-path "..")
(load-file "../alloc-scan.el")

(message "=== TESTING NEW FILE PICKER ===")

;; Test the file finding logic
(let* ((current-file (expand-file-name "../lib/alloc_scan.ml" default-directory))
       (project-root (alloc-scan--find-dune-project current-file))
       (dump-files (when project-root
                     (alloc-scan--find-build-files project-root "\\.cmx\\.dump$"))))
  
  (message "1. File Discovery Test:")
  (message "   Current file: %s" current-file)
  (message "   Project root: %s" project-root)
  (message "   Found %d dump files:" (length dump-files))
  (dolist (file dump-files)
    (message "     - %s" file))
  
  ;; Test the selection logic (without user interaction)
  (message "\n2. Selection Logic Test:")
  (cond
   ((null dump-files)
    (message "   Would prompt for manual file selection"))
   
   ((= (length dump-files) 1)
    (message "   Would offer single file: %s" (file-name-nondirectory (car dump-files))))
   
   (t
    (message "   Would show completion with options:")
    (let ((file-alist (mapcar (lambda (file)
                                (cons (concat (file-name-nondirectory file)
                                            " (" (file-name-directory file) ")")
                                      file))
                              dump-files)))
      (dolist (option file-alist)
        (message "     - %s" (car option)))))))

(message "\n=== FILE PICKER TEST COMPLETE ===")
(message "The new file picker will:")
(message "- Use minibuffer prompts instead of Dired")
(message "- Keep you in your original buffer")
(message "- Show friendly completion options")
(message "- Auto-suggest the most likely dump file")