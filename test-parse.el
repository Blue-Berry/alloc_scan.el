;;; test-parse.el --- Test parsing function -*- lexical-binding: t; -*-

(load-file "alloc-scan.el")

(message "Testing parse-dump-file function...")

(defun test-parse-with-timeout ()
  "Test parsing with a timeout to avoid hanging."
  (let ((dump-file "/home/liam/playground/alloc_scan/_build/default/bin/.main.eobjs/native/dune__exe__Main.cmx.dump")
        (alloc-scan-debug t))
    (message "File readable: %s" (file-readable-p dump-file))
    (with-timeout (5 (message "TIMEOUT: Parsing took too long"))
      (let ((result (alloc-scan--parse-dump-file dump-file)))
        (message "Parse completed successfully")
        (message "Result length: %d" (length result))
        (when result
          (message "First result: %s" (car result)))))))

(test-parse-with-timeout)
(message "Test finished")