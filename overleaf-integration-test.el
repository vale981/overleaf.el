
(require 'cl-lib)
(require 'websocket)
(require 'plz)

(setq overleaf-url "http://localhost:3000")
(setq overleaf-cookies '(("localhost" . ("mock-cookie" 9999999999))))
(setq overleaf-project-id "project1")

(load-file "overleaf.el")

(defun test-overleaf-connection ()
  (with-current-buffer (get-buffer-create "test.tex")
    (insert "\\documentclass{article}\n\\begin{document}\nHello Overleaf!\n\\end{document}")
    (setq buffer-file-name (expand-file-name "test.tex"))
    (setq overleaf-project-id "project1")
    (setq overleaf-document-id "doc1")
    
    (message "Connecting to mock server...")
    (overleaf-mode 1)
    
    ;; Wait for connection
    (let ((count 0))
      (while (and (not (overleaf--connected-p)) (< count 50))
        (accept-process-output nil 0.1)
        (cl-incf count)))
    
    (if (overleaf--connected-p)
        (progn
          (message "Successfully connected!")
          (message "Buffer content: %s" (buffer-string))
          (message "Document version: %s" overleaf--doc-version)
          
          ;; Test edit
          (goto-char (point-max))
          (insert "\n% Test edit")
          (message "Waiting for sync...")
          (accept-process-output nil 2)
          (message "Test finished successfully"))
      (message "Failed to connect to mock server")
      (kill-emacs 1))))

(test-overleaf-connection)
