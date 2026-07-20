(require 'cl-lib)
(load-file "plz.el")
(load-file "websocket.el")

(defun webdriver-firefox-setup (&rest _args))
(defun posframe-show (&rest _args))
(defun posframe-hide (&rest _args))
(provide 'webdriver)
(provide 'webdriver-firefox)
(provide 'posframe)

(load-file "overleaf.el")

(defun overleaf--verify-buffer (hash) nil)
(defun overleaf--update-modeline () nil)

(defun websocket-open (url &rest args) (let ((ws (make-hash-table))) (puthash :open t ws) ws))
(defun websocket-openp (ws) (and ws (gethash :open ws)))
(defun websocket-close (ws) (when ws (puthash :open nil ws)))
(defun websocket-send-text (ws text) nil)

(setq overleaf-url "http://localhost:3999")
(setq overleaf-cookies '(("localhost:3999" . ("mock-cookie" 9999999999))))
(setq overleaf-project-id "dup-test-project")

;; Reproduce the OT test's scenario, but with the local edit ACTUALLY
;; applied to the buffer first (as Emacs really does when the user types),
;; instead of only recorded as a pending op plist.
(with-current-buffer (get-buffer-create "dup-test.tex")
  (erase-buffer)
  (insert "\\documentclass{article}\n\\begin{document}\nHello Overleaf!\n\\end{document}")
  (setq overleaf--buffer (current-buffer))
  (setq-local overleaf-document-id "doc1")
  (setq-local overleaf--doc-version 10)
  (setq-local overleaf--websocket (websocket-open "ws://localhost:3999"))
  (setq-local overleaf--edits-in-flight nil)
  (setq-local overleaf--edit-queue nil)

  (message "Buffer before local edit: %S" (buffer-string))

  ;; Simulate the user actually typing "CONCURRENT_TEST" at position 65,
  ;; the way Emacs would (buffer mutated immediately).
  (let ((pos 65))
    (goto-char (1+ pos))
    (insert "CONCURRENT_TEST")
    (let ((edits (list `(:p ,pos :i "CONCURRENT_TEST"))))
      (setq overleaf--edits-in-flight
            (list (make-overleaf--update
                   :from-version 10 :to-version 11
                   :edits edits :buffer (buffer-string))))))

  (message "Buffer after local edit (in flight, unacked): %S" (buffer-string))

  ;; A remote edit arrives before our ack: insert "SERVER_EDIT 11\n" at pos 0.
  (overleaf--apply-changes '((:p 0 :i "SERVER_EDIT 11\n")) 11 10 "somehash")

  (message "Buffer after remote edit: %S" (buffer-string))

  (let* ((text (buffer-string))
         (count (let ((n 0) (start 0))
                  (while (string-match "CONCURRENT_TEST" text start)
                    (setq n (1+ n) start (match-end 0)))
                  n)))
    (message "Occurrences of CONCURRENT_TEST: %d" count)
    (if (= count 1)
        (progn (message "PASS: local in-flight edit not duplicated") (kill-emacs 0))
      (progn (message "FAIL: local in-flight edit duplicated or lost (count=%d)" count) (kill-emacs 1)))))
