;;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'plz)


(setq inhibit-modification-hooks nil)

(defvar-local overleaf-project-id "6745e47320ed7e0c8f0b1f33")
(defvar-local overleaf-document-id "6745f5dc2ebf8975b0afe402")
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
(defvar-local overleaf-send-interval 3)
(defvar-local overleaf-message-interval .1)
(defvar-local send-edit-queue '())
(defvar-local edit-in-flight nil)
(defvar-local vers 0)


(defun parse-message (ws message)
  (with-current-buffer overleaf--buffer
    (pcase-let ((`(,id ,message-raw) (string-split  message ":::")))
      (pcase id
        ("6"
         (save-match-data
           (when-let* ((doc (progn
                              (string-match "null,\\(\\[.*?\\]\\),\\([0-9]+\\)" message)
                              (match-string 1 message)))

                       (version (1- (string-to-number (match-string 2 message))))
                       (overleaf-change t))
             ;; (message "version %s" version)
             (setq vers version)
             (when doc
               (with-current-buffer overleaf--buffer
                 (let ((point (point)))
                   (save-excursion
                     (setq-local buffer-read-only nil)
                     (erase-buffer)
                     (insert (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n"))))
                   (goto-char point)))))))
        ("5"
         (when message-raw
           (when-let* ((message (json-parse-string message-raw :object-type 'plist :array-type 'list))
                       (name (plist-get message :name)))
             ;; (message ">>>> Message with name: %s" (plist-get message :name))
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
                  (message "aplied")

                  (when (> version vers)

                    (setq vers version)
                    (dolist (op (plist-get (car  (plist-get message :args)) :op))
                      (prin1 op)
                      (goto-char (1+ (plist-get op :p)))
                      (when-let* ((insert (plist-get op :i)))
                        (insert (overleaf--decode-utf8 insert)))
                      (when-let* ((delete (plist-get op :d)))
                        (re-search-forward (regexp-quote delete) nil t)
                        (replace-match "")))
                    ))))))
         ))
      )))

(defun overleaf--decode-utf8 (str)
  (with-temp-buffer
    (call-process "node" nil t nil (concat (file-name-directory (symbol-file 'overleaf--decode-utf8)) "decode.js") str)
    (buffer-string)))



(defun disconnect ()
  (interactive)
  (when wstest-ws
    (setq force-close t)
    (setq send-edit-queue '())
    (websocket-close wstest-ws)
    (setq force-close nil)))

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
       message-timer (progn
                       (when message-timer
                         (cancel-timer message-timer))
                       (run-at-time t overleaf-message-interval #'send-message-from-edit-queue overleaf--buffer)))
      (setq-local
       send-timer
       (progn
         (when send-timer
           (cancel-timer send-timer))
         (run-at-time t overleaf-send-interval #'send-change overleaf--buffer))))))

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
      (setq-local deletion-buffer nil)
      (setq-local send-edit-queue '())
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
    (let ((buff (format "%s" (buffer-substring-no-properties (point-min) (point-max)))))
      (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff)))))


(defun overleaf--escape-string (str)
  (json-encode str))


(defun edit-queue-message (version msg)
  (setq send-edit-queue (nconc send-edit-queue `((,version . ,msg))))
  (setq vers version))

(defun send-message-from-edit-queue (buffer)
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (when (and wstest-ws (websocket-openp wstest-ws) send-edit-queue (not edit-in-flight))
          (let ((current (car send-edit-queue)))
            (setq edit-in-flight (car current))
            (websocket-send-text wstest-ws (cdr current))
            (setq send-edit-queue (cdr send-edit-queue))))))))



(defun send-change (&optional buffer)
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (with-current-buffer overleaf--buffer
        (when (websocket-openp wstest-ws)
          ;; (message "======> %s" last-change-type)
          (websocket-send-text wstest-ws
                               (format "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                                       (current-line) (current-column) overleaf-document-id))
          (pcase last-change-type
            (:d
             (edit-queue-message
              (1+ vers)
              (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":[{\"p\":%i,\"d\":%s}],\"meta\": {\"tc\":\"%s\"},\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                      sequence-id overleaf-document-id overleaf-document-id (- last-change-begin 1)
                      (overleaf--escape-string deletion-buffer) (xah-random-string 18) (1+ vers) vers (get-hash))))
            (:i
             (edit-queue-message
              (1+ vers)
              (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":[{\"p\":%i,\"i\":%s}],\"meta\": {\"tc\":\"%s\"},\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                      sequence-id overleaf-document-id overleaf-document-id (- last-change-begin 1)
                      (overleaf--escape-string (buffer-substring-no-properties last-change-begin last-change-end)) (xah-random-string 18) (1+ vers) vers (get-hash)))))
          (setq last-change-type nil)
          (setq last-change-begin -1)
          (setq last-change-end -1)
          (setq sequence-id (1+ sequence-id))
          (setq deletion-buffer ""))))))

(defun on-change (begin end length)
  ;; (message "INSERT %i %i %s" begin end length)
  (let ((overleaf--buffer (current-buffer)))
    (unless overleaf-change
      (cond
       ((and (> end begin) (not (= (- end begin) length)))
        (unless last-change-type
          (setq last-change-type :i)
          (setq last-change-end end)
          (setq last-change-begin begin))

        (cond
         ((= begin last-change-end)
          (setq last-change-end end))
         ((= end last-change-begin)
          (setq last-change-begin begin))))))))

(defun before-change (begin end)
  (let ((overleaf--buffer (current-buffer)))
    (unless overleaf-change
      (cond
       ;; deletion
       ((< begin end)
        ;; (message "DELETE %i %i %s %s" begin end last-change-begin last-change-end)
        (if (and (eq last-change-type :d) (= end last-change-begin))
            (progn
              (setq last-change-begin begin)
              (setq last-change-end end)
              (setq deletion-buffer (concat (buffer-substring-no-properties begin end) deletion-buffer)))
          (when last-change-type
            (send-change))
          (setq last-change-type :d)
          (setq last-change-begin begin)
          (setq last-change-end end)
          (setq deletion-buffer (buffer-substring-no-properties begin end)))
        ;; (message "DELETION BUFFER: %s" deletion-buffer)
        )

       ;; insertion
       (t
        (unless (or (= last-change-end begin) (= last-change-end begin))
          (send-change)))))))




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
