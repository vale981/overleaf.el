
(require 'cl-lib)

;; Mock dependencies that we can't load easily
(defun websocket-open (url &rest args)
  (message "Mock websocket open to %s" url)
  (let ((ws (make-hash-table)))
    (puthash :url url ws)
    (puthash :open t ws)
    ws))

(defun websocket-openp (ws) (gethash :open ws))
(defun websocket-close (ws) (puthash :open nil ws))
(defun websocket-send-text (ws text) (message "WS Send: %s" text))

(setq overleaf-url "http://localhost:3000")
(setq overleaf-cookies '(("localhost" . ("mock-cookie" 9999999999))))
(setq overleaf-project-id "project1")

;; Mock other required features
(defun posframe-show (&rest _args))
(defun posframe-hide (&rest _args))
(defun webdriver-firefox-setup (&rest _args))

(defun require (pkg &optional file noerror)
  (message "Mock require: %s" pkg))

(load-file "overleaf.el")

;; Test scenario
(defun run-concurrent-edit-test ()
  (with-current-buffer (get-buffer-create "test.tex")
    (erase-buffer)
    (insert "\\documentclass{article}\n\\begin{document}\nHello Overleaf!\n\\end{document}")
    (setq overleaf--buffer (current-buffer))
    (setq-local overleaf-document-id "doc1")
    (setq-local overleaf--doc-version 10)
    (setq-local overleaf--websocket (websocket-open "ws://localhost:3000"))
    
    (message "--- Starting OT Test ---")
    
    ;; 1. Simulate a local edit
    (goto-char (point-max))
    (let ((edit '(:p 65 :i "CONCURRENT_TEST"))) ; This matches our mock server trigger
      (message "Local edit: %S" edit)
      (overleaf--queue-edit edit)
      
      ;; 2. Flush the queue (simulated)
      (let ((edits (mapcar #'copy-sequence overleaf--edit-queue))
            (next-version 11))
        (setq overleaf--edits-in-flight
              (list (make-overleaf--update
                     :from-version 10
                     :to-version 11
                     :edits edits
                     :buffer (buffer-string))))
        (overleaf--reset-edit-queue)
        (message "Edit in flight. version: 10 -> 11"))
      
      ;; 3. Simulate a SERVER edit arriving BEFORE the ACK
      ;; The server edit is: insert "SERVER_EDIT 11\n" at position 0
      (let ((server-edits '((:p 0 :i "SERVER_EDIT 11\n")))
            (server-version 11)
            (last-version 10))
        (message "Server edit received: %S (v %d)" server-edits server-version)
        (overleaf--apply-changes server-edits server-version last-version "somehash"))
      
      (message "Buffer after server edit: %S" (buffer-string))
      ;; Expected: "SERVER_EDIT 11\n\\documentclass..."
      
      ;; 4. Simulate the ACK for the local edit arriving
      (message "ACK received for local edit (v 12)")
      (overleaf--apply-changes nil 12 11 "somehash")
      
      (message "Final buffer content: %S" (buffer-string))
      (message "Final version: %d" overleaf--doc-version)
      
      (if (string-match "SERVER_EDIT 11" (buffer-string))
          (message "Test PASSED: Server edit preserved")
        (message "Test FAILED: Server edit lost"))
      
      (if (string-match "CONCURRENT_TEST" (buffer-string))
          (message "Test PASSED: Local edit preserved")
        (message "Test FAILED: Local edit lost"))
      
      (kill-emacs 0))))

(run-concurrent-edit-test)
