;; -*- lexical-binding: t -*-

(use-package websocket)
(setq websocket-debug t)
(use-package curl-to-elisp)
(require 'websocket)
(use-package s)
(use-package plz)
(setq curlargs "curl 'https://www.overleaf.com/socket.io/1/?projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1&t=1741550282756' -H 'User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:135.0) Gecko/20100101 Firefox/135.0' -H 'Accept: */*' -H 'Accept-Language: en-US,en;q=0.5' -H 'Accept-Encoding: gzip, deflate, br, zstd' -H 'Referer: https://www.overleaf.com/project/67cb6eea861396a27bbc7aab' -H 'Connection: keep-alive' -H 'Cookie: overleaf_session2=s%3A11Va3hIwoXyaWUF_7geULEBGH5dUHPMr.rgQT%2FXONrClS0o0OyW1p4a4Kormb2v6UyC86jtY%2FMR0; __stripe_mid=ed98fee4-3a6c-4417-bac5-2b0855311097295942; GCLB=CMD37ejsnuTxkAEQAw; __stripe_sid=51ed3af8-6b52-4e05-a7c3-ece7c593bcd5c4a119' -H 'Sec-Fetch-Dest: empty' -H 'Sec-Fetch-Mode: cors' -H 'Sec-Fetch-Site: same-origin' -H 'TE: trailers'")


(setq url (save-match-data
            (string-match "curl '\\(.*?\\)'" curlargs)
            (match-string 1 curlargs)))
(setq cookies (save-match-data
                (string-match "-H 'Cookie: \\(.*?\\)'" curlargs)
                (match-string 1 curlargs)))

(setq test-buffer (get-buffer-create "overtest"))
(setq headers
      (let ((matches '())
            (start 0))
        (while (string-match "-H '\\(.*?\\):\\(.*?\\)'" curlargs start)
          (let ((name (match-string 1 curlargs))
                (value (match-string 2 curlargs))) ; Capture group 1
            (push `(,name . ,value) matches))
          (setq start (match-end 0)))   ; Move past the last match
        matches))



(setq vers 0)
(setq hash "")
(defun parse-message (ws message)
  (pcase-let ((`(,id ,message-raw) (string-split  message ":::")))
    (pcase id
      ("6"
       (save-match-data
         (let ((doc (progn
                      (string-match "null,\\(\\[.*?\\]\\),\\([0-9]+\\)" message)
                      (match-string 1 message)))

               (version (1- (string-to-number (match-string 2 message))))
               (overleaf-change t))
           ;; (message "version %s" version)
           (setq vers version)
           (when doc
             (with-current-buffer test-buffer
               (erase-buffer)
               (insert (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n"))))))))
      ("5"
       (let ((message (json-parse-string message-raw :object-type 'plist :array-type 'list)))
         (message ">>>> Message with name: %s" (plist-get message :name))
         (pcase (plist-get message :name)
           ("otUpdateError"
            (prin1  (plist-get message :args)))
           ("joinProjectResponse"
            (websocket-send-text ws "5:2+::{\"name\":\"joinDoc\",\"args\":[\"67cb6eea861396a27bbc7ab0\",{\"encodeRanges\":true}]}"))
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
                (with-current-buffer test-buffer
                  (dolist (op (plist-get (car  (plist-get message :args)) :op))
                    (prin1 op)
                    (goto-char (1+ (plist-get op :p)))
                    (when-let ((insert (plist-get op :i)))
                      (insert (overleaf--decode-utf8 insert)))
                    (when-let ((delete (plist-get op :d)))
                      (re-search-forward (regexp-quote delete) nil t)
                      (replace-match ""))))
                )))))
       ))
    ))

(defun overleaf--decode-utf8 (str)
  (with-temp-buffer
    (call-process "node" nil t nil "decode.js" str)
    (buffer-string)))

(defun onmess (ws frame)
  ;; (prin1 frame)
  ;; (message "============")
  (parse-message ws (websocket-frame-text frame)))

(setq inhibit-modification-hooks nil)
(defvar overleaf-change nil)
(defvar force-close nil)
(defvar wstest-ws nil)

(defvar last-change-type nil)
(defvar last-change-begin 0)
(defvar last-change-end 0)
(defvar deletion-buffer "")
(defvar send-timer nil)
(defvar message-timer nil)
(defvar overleaf-send-interval 3)
(defvar overleaf-message-interval .1)

(defvar send-edit-queue '())

(defun connect-over ()
  (interactive)
  (when wstest-ws
    (setq force-close t)
    (websocket-close wstest-ws)
    (setq force-close nil))
  (setq last-change-type nil)
  (setq deletion-buffer nil)
  (setq send-edit-queue '())
  (setq edit-in-flight nil)
  (let ((ws-id
         (car (string-split
               (plz 'get "https://www.overleaf.com/socket.io/1/?projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1"
                 :headers `(("Cookie" . ,cookies))) ":"))))
    (setq vers 0)
    (setq sequence-id 2)
    (setq wstest-ws
          (websocket-open
           (concat "wss://www.overleaf.com/socket.io/1/websocket/" ws-id "?projectId=67cb6eea861396a27bbc7aab&esh=1&ssp=1")
           :on-message #'onmess
           :on-close (lambda (_websocket)
                       (unless force-close
                         (connect-over)))
           :on-open (lambda (_websocket) )
           :custom-header-alist `(("Cookie" . ,cookies))))))
(connect-over)

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
  (with-current-buffer test-buffer
    (let ((buff (format "%s" (buffer-substring-no-properties (point-min) (point-max)))))
      (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff)))))


(defun overleaf--escape-string (str)
  (json-encode str))


(defun edit-queue-message (version msg)
  (setq send-edit-queue (nconc send-edit-queue `((,version . ,msg))))
  (setq vers version))

(defun send-message-from-edit-queue ()
  (when (and send-edit-queue (not edit-in-flight))
    (let ((current (car send-edit-queue)))
      (setq edit-in-flight (car current))
      (websocket-send-text wstest-ws (cdr current))
      (setq send-edit-queue (cdr send-edit-queue)))))
(defvar edit-in-flight nil)

(setq message-timer
      (progn
        (when message-timer
          (cancel-timer message-timer))
        (run-at-time t overleaf-message-interval #'send-message-from-edit-queue)))


(defun send-change ()
  (with-current-buffer test-buffer
    ;; (message "======> %s" last-change-type)
    (websocket-send-text wstest-ws
                         (format "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"67cb6eea861396a27bbc7ab0\"}]}"
                                 (current-line) (current-column)))
    (pcase last-change-type
      (:d
       (edit-queue-message
        (1+ vers)
        (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"67cb6eea861396a27bbc7ab0\",{\"doc\":\"67cb6eea861396a27bbc7ab0\",\"op\":[{\"p\":%i,\"d\":%s}],\"meta\": {\"tc\":\"%s\"},\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                sequence-id (- last-change-begin 1)
                (overleaf--escape-string deletion-buffer) (xah-random-string 18) (1+ vers) vers (get-hash))))
      (:i
       (edit-queue-message
        (1+ vers)
        (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"67cb6eea861396a27bbc7ab0\",{\"doc\":\"67cb6eea861396a27bbc7ab0\",\"op\":[{\"p\":%i,\"i\":%s}],\"meta\": {\"tc\":\"%s\"},\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                sequence-id (- last-change-begin 1)
                (overleaf--escape-string (buffer-substring-no-properties last-change-begin last-change-end)) (xah-random-string 18) (1+ vers) vers (get-hash)))))
    (setq last-change-type nil)
    (setq last-change-begin -1)
    (setq last-change-end -1)
    (setq sequence-id (1+ sequence-id))
    (setq deletion-buffer "")))

(setq send-timer
      (progn
        (when send-timer
          (cancel-timer send-timer))
        (run-at-time t overleaf-send-interval #'send-change)))

(with-current-buffer test-buffer
  (goto-char last-change-begin))

(progn
  (defun on-change (begin end length)
    ;; (message "INSERT %i %i %s" begin end length)

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
          (setq last-change-begin begin)))))))
  (defun before-change (begin end)
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
          (send-change))))))

  (with-current-buffer test-buffer
    (add-hook 'after-change-functions #'on-change nil t)
    (add-hook 'before-change-functions #'before-change nil t)))




(defun my-clear-messages-buffer ()
  "Clear the contents of the *Messages* buffer if it is the current buffer."
  (with-current-buffer (messages-buffer)
    (let ((was-read-only buffer-read-only))
      (when was-read-only
        (read-only-mode -1))
      (erase-buffer)
      (when was-read-only
        (read-only-mode 1)))))

(my-clear-messages-buffer)
(connect-over)
(send-change)
