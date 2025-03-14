;;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'plz)


(setq inhibit-modification-hooks nil)

(defvar-local overleaf-project-id "6745e47320ed7e0c8f0b1f33")
(defvar-local overleaf-document-id "67d1e0943c6e2e8fec64bd81")
(defvar-local overleaf-cookies "overleaf_session2=s%3A11Va3hIwoXyaWUF_7geULEBGH5dUHPMr.rgQT%2FXONrClS0o0OyW1p4a4Kormb2v6UyC86jtY%2FMR0; __stripe_mid=ed98fee4-3a6c-4417-bac5-2b0855311097295942; GCLB=CMD37ejsnuTxkAEQAw; __stripe_sid=51ed3af8-6b52-4e05-a7c3-ece7c593bcd5c4a119")
(defvar-local overleaf-url "https://www.overleaf.com")
(defvar overleaf--buffer nil)
(defvar-local overleaf-change nil)
(defvar-local force-close nil)
(defvar-local wstest-ws nil)
(defvar-local last-change-type nil)
(defvar-local last-change-begin 0)
(defvar-local last-change-end 0)
(defvar-local deletion-buffer "")
(defvar-local send-timer nil)
(defvar-local message-timer nil)
(defvar-local overleaf-send-interval .5)
(defvar-local overleaf-message-interval .1)
(defvar-local send-edit-queue '())
(defvar-local send-message-queue '())
(defvar-local edit-in-flight nil)
(defvar-local vers 0)


(defun parse-message (ws message)
  (with-current-buffer overleaf--buffer
    (pcase-let ((`(,id ,message-raw) (string-split message ":::")))
      (pcase id
        ("2::"
         (websocket-send-text ws "2::"))
        ("6"
         (save-match-data
           (when-let* ((doc (progn
                              (string-match "null,\\(\\[.*?\\]\\),\\([0-9]+\\)" message)
                              (match-string 1 message)))

                       (version (1- (string-to-number (match-string 2 message))))
                       (overleaf-change t))
             (setq vers version)
             (when doc
               (let ((point (point)))
                 (setq-local buffer-read-only nil)
                 (erase-buffer)
                 (insert (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n")))
                 (goto-char point)
                 (setq buffer-undo-list nil))))))
        ("5"
         (when message-raw
           (when-let* ((message (json-parse-string message-raw :object-type 'plist :array-type 'list))
                       (name (plist-get message :name)))

             (pcase name
               ("otUpdateError"
                (prin1  (plist-get message :args)))
               ("joinProjectResponse"
                (websocket-send-text ws (format "5:2+::{\"name\":\"joinDoc\",\"args\":[\"%s\",{\"encodeRanges\":true}]}" overleaf-document-id)))
               ("serverPing"
                ;; (message "=========> PONG" )
                (let ((res (concat id ":::" (json-encode `(:name "clientPong" :args ,(plist-get message :args))))))
                  (websocket-send-text ws res)))
               ("otUpdateApplied"
                (let ((version (plist-get (car (plist-get message :args)) :v))
                      (overleaf-change t))
                  (when (eq edit-in-flight version)
                    (setq edit-in-flight nil))

                  (when (> version vers)
                    (setq vers version)
                    (undo-boundary)
                    (dolist (op (plist-get (car  (plist-get message :args)) :op))
                      (goto-char (1+ (plist-get op :p)))
                      (when-let* ((insert (plist-get op :i)))
                        (insert (overleaf--decode-utf8 insert)))
                      (when-let* ((delete (plist-get op :d)))
                        (re-search-forward (regexp-quote delete) nil t)
                        (replace-match ""))))
                  (setq buffer-undo-list (memq nil buffer-undo-list)))))))
         ))
      )))

(defun overleaf--decode-utf8 (str)
  (with-temp-buffer
    (call-process "node" nil t nil (concat (file-name-directory (symbol-file 'overleaf--decode-utf8)) "decode.js") str)
    (buffer-string)))



(defun disconnect ()
  (interactive)
  (when wstest-ws
    (setq-local force-close t)
    (setq-local send-edit-queue '())
    (setq-local send-message-queue '())
    (websocket-close wstest-ws)
    (setq-local force-close nil)))

(defun on-close (_websocket)
  (let ((overleaf--buffer (gethash (websocket-url _websocket) buffer-ws-table)))
    (with-current-buffer overleaf--buffer
      (cancel-timer message-timer)
      (cancel-timer send-timer)
      (remhash (websocket-url _websocket) buffer-ws-table)
      (unless force-close
        (with-current-buffer overleaf--buffer
          (setq buffer-read-only t)
          (sleep-for .1)
          (setq wstest-ws nil)
          (connect-over))))))

(defun on-message (ws frame)
  (let ((overleaf--buffer (gethash (websocket-url ws) buffer-ws-table)))
    (parse-message ws (websocket-frame-text frame))))

(defun on-open (_websocket)
  (let ((overleaf--buffer (gethash (websocket-url _websocket) buffer-ws-table)))
    (with-current-buffer overleaf--buffer
      (add-hook 'after-change-functions #'on-change nil t)
      (add-hook 'before-change-functions #'before-change nil t)

      (setq-local
       message-timer
       (progn
         (when message-timer
           (cancel-timer message-timer))
         (run-at-time t overleaf-message-interval #'send-queued-message overleaf--buffer)))

      (setq-local
       send-timer
       (progn
         (when send-timer
           (cancel-timer send-timer))
         (run-with-idle-timer overleaf-send-interval t #'send-message-from-edit-queue overleaf--buffer))))))

(defvar buffer-ws-table (make-hash-table :test #'equal))

(defun connect-over ()
  (interactive)
  (disconnect)
  (let ((overleaf--buffer (current-buffer))
        (ws-id
         (car (string-split
               (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" overleaf-url overleaf-project-id)
                 :headers `(("Cookie" . ,overleaf-cookies))) ":"))))

    (with-current-buffer overleaf--buffer
      (setq-local last-change-type nil)
      (setq-local deletion-buffer "")
      (setq-local send-edit-queue '())
      (setq-local send-message-queue '())
      (setq-local edit-in-flight nil)
      (setq-local buffer-read-only t)
      (setq-local vers 0)
      (setq-local sequence-id 2)
      (puthash (websocket-url (setq-local wstest-ws
                                          (websocket-open
                                           (replace-regexp-in-string "https" "wss" (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1" overleaf-url ws-id overleaf-project-id))
                                           :on-message #'on-message
                                           :on-close #'on-close
                                           :on-open #'on-open
                                           :custom-header-alist `(("Cookie" . ,overleaf-cookies)))))
               overleaf--buffer
               buffer-ws-table))))

(defun xah-random-string (&optional CountX)
  "return a random string of length CountX.
The possible chars are: 2 to 9, upcase or lowercase English alphabet but no a e i o u, no L l and no 0 1.

URL `http://xahlee.info/emacs/emacs/elisp_insert_random_number_string.html'
Version: 2024-04-03"
  (interactive )
  (let ((xcharset "abcdfghjkmnpqrstvwxyz23456789") xcount xvec)
    (setq xcount (length xcharset))
    (setq xvec (mapcar (lambda (_) (aref xcharset (random xcount))) (make-vector (if CountX CountX 5) 0)))
    (mapconcat 'char-to-string xvec)))

(defun get-hash ()
  (with-current-buffer overleaf--buffer
    (save-restriction
      (widen)
      (let ((buff (buffer-string)))
        (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff))))))


(defun overleaf--escape-string (str)
  (json-encode str))

(defun edit-queue-message (edit)
  (setq send-edit-queue (nconc send-edit-queue (list edit))))

(defun queue-message (message version)
  (setq send-message-queue (nconc send-message-queue `((,message . ,version)))))

(defun send-queued-message (&optional buffer)
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (unless edit-in-flight
        (with-current-buffer overleaf--buffer
          (when send-message-queue
            (when-let* ((message (car send-message-queue)))
              (setq-local edit-in-flight (cdr message))
              (websocket-send-text wstest-ws (car message))
              (websocket-send-text wstest-ws
                                   (format "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                                           (current-line) (current-column) overleaf-document-id))

              (setq send-message-queue (cdr send-message-queue)))))))))

(defun queue-change (&optional buffer)
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (with-current-buffer overleaf--buffer
        (when (and last-change-type (websocket-openp wstest-ws))
          ;; (message "======> %s %i %i %s" last-change-type last-change-begin (point) deletion-buffer)
          (pcase last-change-type
            (:d
             (edit-queue-message
              `(:p ,(- last-change-begin 1) :d ,deletion-buffer)))
            (:i
             (edit-queue-message
              `(:p ,(- last-change-begin 1) :i ,(buffer-substring-no-properties last-change-begin last-change-end)))))
          (setq-local last-change-type nil)
          (setq-local last-change-begin -1)
          (setq-local last-change-end -1)
          (setq-local deletion-buffer ""))))))

(defun send-message-from-edit-queue (buffer)
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (queue-change)
        (when (and wstest-ws (websocket-openp wstest-ws) send-edit-queue)
          (setq-local buffer-read-only t)
          (queue-message
           (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s,\"meta\": {\"tc\":\"%s\"},\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                   sequence-id
                   overleaf-document-id
                   overleaf-document-id
                   (json-encode (apply #'vector send-edit-queue))
                   (xah-random-string 18) (1+ vers) vers (get-hash))
           (1+ vers))
          (setq-local sequence-id (1+ sequence-id))
          (setq-local vers (1+ vers))
          (setq send-edit-queue '())
          (setq-local buffer-read-only nil))))))


(defvar-local overleaf--before-change "")
(defvar-local overleaf--before-change-begin -1)

(defun on-change (begin end length)
  ;; (message "after %i %i %s %S" begin end length last-change-type)
  (let ((overleaf--buffer (current-buffer)))
    (unless overleaf-change
      (let ((new (buffer-substring-no-properties begin end)))
        ;; (message "%s %s" (json-encode-string overleaf--before-change) (json-encode-string new))
        (unless (and (equal new overleaf--before-change))
          (let ((empty-before (equal overleaf--before-change ""))
                (empty-after (equal new "")))

            (cond
             (empty-before
              (let ((begin-matches (= begin last-change-end))
                    (end-matches (= end last-change-begin)))
                ;; (message "insert \"%s\"" new)

                (if (or (not last-change-type) (eq last-change-type :d) (not (or begin-matches end-matches)))
                    (progn
                      (setq last-change-type :i)
                      (setq last-change-end end)
                      (setq last-change-begin begin))

                  (setq last-change-type :i)

                  ;; extend
                  (cond
                   (begin-matches
                    (setq last-change-end end))
                   (end-matches
                    (setq last-change-begin begin))))))
             (empty-after
              ;; (when (and last-change-type (not (eq last-change-type :d)))
              ;;   (message "NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"))

              ;; (message "del" )

              (unless last-change-type
                (setq-local last-change-begin overleaf--before-change-begin)
                (setq-local last-change-end end))

              (setq last-change-type :d)
              (if (> last-change-begin  overleaf--before-change-begin)
                  (progn
                    (setq last-change-begin overleaf--before-change-begin)
                    (setq-local deletion-buffer (concat overleaf--before-change deletion-buffer)))
                (setq-local deletion-buffer (concat deletion-buffer overleaf--before-change))
                (setq last-change-end end))
              (setq-local last-change-begin overleaf--before-change-begin))
             (t
              (let ((overleaf-change t))
                (delete-region begin end)
                (save-excursion
                  (goto-char overleaf--before-change-begin)
                  (insert overleaf--before-change)))
              (let ((overleaf-change nil))
                (delete-region overleaf--before-change-begin overleaf--before-change-end))

              (setq-local last-change-begin overleaf--before-change-begin)
              (setq-local last-change-end overleaf--before-change-end)
              (setq-local last-change-type :d)
              (setq-local deletion-buffer overleaf--before-change)
              (queue-change)
              (send-message-from-edit-queue overleaf--buffer)
              (goto-char overleaf--before-change-begin)
              (setq-local last-change-begin begin)
              (setq-local last-change-end end)
              (setq-local last-change-type :i)
              (insert new)
              (queue-change)
              (send-message-from-edit-queue overleaf--buffer)))))))))

(defvar-local overleaf--before-change-end -1)

(defun before-change (begin end)
  (unless overleaf-change
    (let ((overleaf--buffer (current-buffer)))
      (setq-local overleaf--before-change (buffer-substring-no-properties begin end))
      (setq-local overleaf--before-change-begin begin)
      (setq-local overleaf--before-change-end end)
      (when
          (or
           (not (or (= begin last-change-begin) (= begin last-change-end) (= end last-change-begin) (= end last-change-end)))
           (and (= end begin) (or (eq last-change-type :d)))
           (and (eq last-change-type :i) (or (> end begin) (> (length overleaf--before-change) 0))))
        ;; (message "QUEUE %i %i %i %i %S" begin end last-change-begin last-change-end last-change-type)
        (send-message-from-edit-queue (current-buffer))))))




(defun my-clear-messages-buffer ()
  "Clear the contents of the *Messages* buffer if it is the current buffer."
  (interactive)
  (with-current-buffer (messages-buffer)
    (let ((was-read-only buffer-read-only))
      (when was-read-only
        (read-only-mode -1))
      (erase-buffer)
      (when was-read-only
        (read-only-mode 1)))))

;; (setq tracker nil)
;; (defun test-track (a b c)
;;   (message "%i %i %s" a b c))
;; (defun test-sig (a &optional b )
;;   (unless tracker
;;     (setq tracker a))
;;   (track-changes-fetch tracker #'test-track)
;;   t)

;; (with-current-buffer (get-buffer-create "test")
;;   (track-changes-register #'test-sig))
