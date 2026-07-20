
(require 'cl-lib)

;; Load real libraries from current directory
(load-file "plz.el")
(load-file "websocket.el")

;; Mock some bits that are hard to run in batch
(defun webdriver-firefox-setup (&rest _args))
(defun posframe-show (&rest _args))
(defun posframe-hide (&rest _args))
(provide 'webdriver)
(provide 'webdriver-firefox)
(provide 'posframe)

(load-file "overleaf.el")

;; Mock these AFTER loading overleaf.el
(defun overleaf--verify-buffer (hash) nil)
(defun overleaf--get-hash (&optional _buffer) "hash")
(defun overleaf--update-modeline () nil)

;; Keep websocket mocked
(defun websocket-open (url &rest args)
  (let ((ws (make-hash-table)))
    (puthash :url url ws)
    (puthash :open t ws)
    ws))
(defun websocket-openp (ws) (and ws (gethash :open ws)))
(defun websocket-close (ws) (when ws (puthash :open nil ws)))
(defun websocket-send-text (ws text) nil)

(setq overleaf-url "http://localhost:3001")
(setq overleaf-cookies '(("localhost:3001" . ("mock-cookie" 9999999999))))
(setq overleaf-project-id "stress-project")

(defun run-chaos-stress-test ()
  (with-current-buffer (get-buffer-create "stress.tex")
    (erase-buffer)
    (insert "Initial content\n" (make-string 1000 ?\n))
    (setq-local overleaf--buffer (current-buffer))
    (setq-local overleaf-document-id "stress-doc")
    (setq-local overleaf--doc-version 0)
    (setq-local overleaf--websocket (websocket-open "ws://localhost:3001"))
    (setq-local overleaf--user-positions (make-hash-table :test #'equal))
    (setq-local overleaf--edits-in-flight nil)
    (setq-local overleaf--edit-queue nil)
    
    (message "--- Starting Chaos Stress Test ---")
    
    (let ((iterations 1000)
          (failed nil))
      (condition-case err
          (dotimes (i iterations)
            ;; 1. Random Local Edit
            (let* ((pos (1+ (random (buffer-size))))
                   (chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789\n")
                   (text (string (aref chars (random (length chars))))))
              (goto-char pos)
              (let ((overleaf--is-overleaf-change nil))
                (if (and (> (buffer-size) 10) (= 0 (random 10)))
                    (let ((len (min 5 (1- (- (buffer-size) pos)))))
                      (when (> len 0)
                        (delete-region pos (+ pos len))
                        (overleaf--queue-edit `(:p ,(1- pos) :d ,(buffer-substring-no-properties pos (+ pos len))))))
                  (insert text)
                  (overleaf--queue-edit `(:p ,(1- pos) :i ,text))))
              
              ;; 2. Manually trigger flushing sometimes
              (when (= 0 (mod i 3))
                (let ((edits (mapcar #'copy-sequence overleaf--edit-queue))
                      (next-version (1+ overleaf--doc-version)))
                  (when edits
                    (setq overleaf--edits-in-flight
                          (nconc overleaf--edits-in-flight
                                 (list (make-overleaf--update
                                        :from-version overleaf--doc-version
                                        :to-version next-version
                                        :edits edits
                                        :buffer (buffer-string)))))
                    (overleaf--reset-edit-queue)
                    (overleaf--set-version next-version))))
              
              ;; 3. Simulate Incoming Server Edits
              (when (= 0 (mod i 2))
                (let* ((spos (1+ (random (buffer-size))))
                       (stext " chaos ")
                       (sversion (1+ overleaf--doc-version)))
                  (overleaf--apply-changes `((:p ,(1- spos) :i ,stext)) sversion overleaf--doc-version "hash")))
              
              ;; 4. Occasional ACK
              (when (and overleaf--edits-in-flight (= 0 (mod i 5)))
                (let ((update (pop overleaf--edits-in-flight)))
                  (overleaf--apply-changes nil (overleaf--update-to-version update) (overleaf--update-from-version update) "hash")))))
        (error
         (message "STRESS TEST CRASHED: %S" err)
         (setq failed t)))
      
      (if failed
          (progn
            (message "Test Failed")
            (kill-emacs 1))
        (progn
          (message "--- Stress Test Finished Successfully ---")
          (message "Final Buffer Size: %d" (buffer-size))
          (message "Final Version: %d" overleaf--doc-version)
          (message "Remaining In-flight: %d" (length overleaf--edits-in-flight))
          (kill-emacs 0))))))

(run-chaos-stress-test)
