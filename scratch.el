;;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'plz)

(defcustom overleaf-keymap-prefix "C-c C-o"
  "The prefix for dotcrafter-mode key bindings."
  :type 'string
  :group 'overleaf-connection-mode)

(defvar overleaf-cookies nil
  "The overleaf session cookies.

Can a string or function that returns a string containing the overleaf
authentication cookies.

For example the variable can be bound to a function that loads the
cookies from a gpg encrypted file.

The cookies are most easily obtained from the developer tools in the
browser.")

(defcustom overleaf-url "https://www.overleaf.com"
  "The url of the overleaf server."
  :type 'string
  :group 'overleaf-connection-mode)

(defcustom overleaf-flush-interval .5
  "The idle-timer delay to flush the edit queue."
  :type 'float
  :group 'overleaf-connection-mode)

(defcustom overleaf-message-interval .1
  "The interval for the timer that syncs changes to overleaf."
  :type 'float
  :group 'overleaf-connection-mode)

(defvar-local overleaf-project-id nil
  "The overleaf project id.

When having a project opened in the browser the URL should read
\"https://[overleaf-domain]/project/[project-id]\".
")

(defvar-local overleaf-document-id nil
  "The overleaf document id as a string.

The id is most easily obtained by downloading the file that is to be
edited from the overleaf interface. The download URL will then be of the form
\"https://[overleaf-domain]/project/[project-id]/doc/[document-id]\".")

(defvar-local overleaf-track-changes nil
  "Whether or not to track changes in overleaf.")


(defvar-local overleaf--is-overleaf-change nil
  "Is set to `t` if the current change in the buffer comes from overleaf.
Used to inhibit the change detection.")

(defvar-local overleaf--force-close nil
  "If `t` the connection will not be reestablished upon disconnection.")

(defvar-local overleaf--websocket nil
  "The websocket instance connected to overleaf.")


(defvar-local overleaf--last-change-type nil)
(defvar-local overleaf--last-change-begin 0)
(defvar-local overleaf--last-change-end 0)
(defvar-local overleaf--deletion-buffer "")
(defvar-local overleaf--flush-edit-queue-timer nil
  "A timer that flushes the edit queue.

See `overleaf--edit-queue'.")

(defvar-local overleaf--message-timer nil
  "A timer that sends edits from the message queue if it isn't empty.

See `overleaf--message-queue'.")

(defvar-local overleaf--edit-queue '()
  "A list of edits that can be send in one go.")

(defvar-local send-message-queue '()
  "A list of messages containing edits that have yet to be synced to overleaf.

See `overleaf--message-timer'.")

(defvar-local overleaf--edit-in-flight nil
  "If non-nil, we still await the acknowledgment from overleaf.")

(defvar-local overleaf--doc-version -1
  "Current version of the document.")

(defvar-local overleaf--mode-line ""
  "Contents of the mode-line indicator.")

(defvar overleaf--buffer-ws-table (make-hash-table :test #'equal)
  "A hash table associating web-sockets to buffers.")

(defvar overleaf--buffer
  "The current overleaf buffer (used in lexical binding).")

(defun overleaf--connected-p ()
  "Return t if the buffer is connected to overleaf."
  (and overleaf--websocket
       (websocket-openp overleaf--websocket)
       (>= overleaf--doc-version 0)))

(defun overleaf--write-buffer-variables ()
  "Write the current buffer-local variables to the buffer."
  (when (overleaf--connected-p)
    (let ((overleaf--is-overleaf-change nil)
          (track-changes overleaf-track-changes))
      (setq-local overleaf-track-changes nil)
      (add-file-local-variable 'overleaf-document-id overleaf-document-id)
      (add-file-local-variable 'overleaf-project-id overleaf-project-id)
      (add-file-local-variable 'overleaf-track-changes track-changes)
      (overleaf--flush-edit-queue (current-buffer))
      (setq-local overleaf-track-changes track-changes))))

(defun parse-message (ws message)
  "Parse a message MESSAGE from overleaf, responding by writing to WS."
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
                       (overleaf--is-overleaf-change t))
             (setq overleaf--doc-version version)
             (when doc
               (let ((point (point)))
                 (setq-local buffer-read-only nil)
                 (erase-buffer)
                 (insert (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n")))
                 (overleaf--write-buffer-variables)
                 (goto-char point)
                 (setq buffer-undo-list nil)))))
         (overleaf--update-modeline))
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
                      (overleaf--is-overleaf-change t))
                  (when (eq overleaf--edit-in-flight version)
                    (setq overleaf--edit-in-flight nil))

                  (when (> version overleaf--doc-version)
                    (setq overleaf--doc-version version)
                    (undo-boundary)
                    (dolist (op (plist-get (car  (plist-get message :args)) :op))
                      (goto-char (1+ (plist-get op :p)))
                      (when-let* ((insert (plist-get op :i)))
                        (insert (overleaf--decode-utf8 insert)))
                      (when-let* ((delete (plist-get op :d)))
                        (re-search-forward (regexp-quote delete) nil t)
                        (replace-match "")))
                    (setq buffer-undo-list (memq nil buffer-undo-list))))))))
         ))
      )))

(defun overleaf--decode-utf8 (str)
  (with-temp-buffer
    (call-process "node" nil t nil (concat (file-name-directory (symbol-file 'overleaf--decode-utf8)) "decode.js") str)
    (buffer-string)))



(defun disconnect ()
  (interactive)
  (when overleaf--websocket
    (setq-local overleaf--force-close t)
    (setq-local overleaf--edit-queue '())
    (setq-local send-message-queue '())
    (websocket-close overleaf--websocket)
    (setq-local overleaf--force-close nil)))

(defun on-close (_websocket)
  (let ((overleaf--buffer (gethash (websocket-url _websocket) overleaf--buffer-ws-table)))
    (with-current-buffer overleaf--buffer
      (when overleaf--websocket
        (setq-local buffer-read-only nil)
        (cancel-timer overleaf--message-timer)
        (cancel-timer overleaf--flush-edit-queue-timer)
        (remhash (websocket-url _websocket) overleaf--buffer-ws-table)
        (setq-local overleaf--websocket nil)
        (overleaf--update-modeline)
        (unless overleaf--force-close
          (with-current-buffer overleaf--buffer
            (setq buffer-read-only t)
            (sleep-for .1)
            (setq overleaf--websocket nil)
            (connect-over)))))))

(defun on-message (ws frame)
  (let ((overleaf--buffer (gethash (websocket-url ws) overleaf--buffer-ws-table)))
    (parse-message ws (websocket-frame-text frame))))

(defun on-open (_websocket)
  (let ((overleaf--buffer (gethash (websocket-url _websocket) overleaf--buffer-ws-table)))
    (with-current-buffer overleaf--buffer
      (overleaf--update-modeline)

      (add-hook 'after-change-functions #'on-change nil t)
      (add-hook 'before-change-functions #'before-change nil t)

      (setq-local
       overleaf--message-timer
       (progn
         (when overleaf--message-timer
           (cancel-timer overleaf--message-timer))
         (run-at-time t overleaf-message-interval #'send-queued-message overleaf--buffer)))

      (setq-local
       overleaf--flush-edit-queue-timer
       (progn
         (when overleaf--flush-edit-queue-timer
           (cancel-timer overleaf--flush-edit-queue-timer))
         (run-with-idle-timer overleaf-flush-interval t #'overleaf--flush-edit-queue overleaf--buffer))))))


(defun overleaf--get-cokies ()
  "Load the cookies either directly as a string from `overleaf-cookies` or by calling the function bound to the symbol."
  (if (or (functionp overleaf-cookies)
          (fboundp 'overleaf-cookies))
      (funcall overleaf-cookies)
    overleaf-cookies))

(defun connect-over ()
  (interactive)
  (overleaf-connection-mode t)
  (disconnect)
  (if overleaf-cookies
      (let*
          ((overleaf--buffer (current-buffer))
           (cookies (overleaf--get-cokies))
           (ws-id
            (car (string-split
                  (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" overleaf-url overleaf-project-id)
                    :headers `(("Cookie" . ,cookies))) ":"))))

        (with-current-buffer overleaf--buffer
          (setq overleaf-project-id
                (or overleaf-project-id
                    (read-from-minibuffer "Overleaf project id: ")))
          (setq overleaf-document-id
                (or overleaf-document-id
                    (read-from-minibuffer "Overleaf document id: ")))

          (setq-local overleaf--last-change-type nil)
          (setq-local overleaf--deletion-buffer "")
          (setq-local overleaf--edit-queue '())
          (setq-local send-message-queue '())
          (setq-local overleaf--edit-in-flight nil)
          (setq-local buffer-read-only t)
          (setq-local overleaf--doc-version -1)
          (setq-local sequence-id 2)
          (puthash
           (websocket-url
            (setq-local overleaf--websocket
                        (websocket-open
                         (replace-regexp-in-string
                          "https" "wss"
                          (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                                  overleaf-url ws-id overleaf-project-id))
                         :on-message #'on-message
                         :on-close #'on-close
                         :on-open #'on-open
                         :custom-header-alist `(("Cookie" . ,cookies)))))
           overleaf--buffer
           overleaf--buffer-ws-table)
          (overleaf--update-modeline)))
    (error "Please set `overleaf-cookies`.")))

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
  (setq overleaf--edit-queue (nconc overleaf--edit-queue (list edit))))

(defun queue-message (message version)
  (setq send-message-queue (nconc send-message-queue `((,message . ,version)))))

(defun send-queued-message (&optional buffer)
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (unless overleaf--edit-in-flight
        (with-current-buffer overleaf--buffer
          (when send-message-queue
            (when-let* ((message (car send-message-queue)))
              (setq-local overleaf--edit-in-flight (cdr message))
              (websocket-send-text overleaf--websocket (car message))
              (websocket-send-text overleaf--websocket
                                   (format "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                                           (current-line) (current-column) overleaf-document-id))

              (setq send-message-queue (cdr send-message-queue)))))))))

(defun queue-change (&optional buffer)
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (with-current-buffer overleaf--buffer
        (when (and overleaf--last-change-type (websocket-openp overleaf--websocket))
          ;; (message "======> %s %i %i %s" overleaf--last-change-type overleaf--last-change-begin (point) overleaf--deletion-buffer)
          (pcase overleaf--last-change-type
            (:d
             (edit-queue-message
              `(:p ,(- overleaf--last-change-begin 1) :d ,overleaf--deletion-buffer)))
            (:i
             (edit-queue-message
              `(:p ,(- overleaf--last-change-begin 1) :i ,(buffer-substring-no-properties overleaf--last-change-begin overleaf--last-change-end)))))
          (setq-local overleaf--last-change-type nil)
          (setq-local overleaf--last-change-begin -1)
          (setq-local overleaf--last-change-end -1)
          (setq-local overleaf--deletion-buffer ""))))))

(defun overleaf--flush-edit-queue (buffer)
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (queue-change)
        (when (and overleaf--websocket (websocket-openp overleaf--websocket) overleaf--edit-queue)
          (setq-local buffer-read-only t)
          (queue-message
           (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s%s,\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                   sequence-id
                   overleaf-document-id
                   overleaf-document-id
                   (json-encode (apply #'vector overleaf--edit-queue))
                   (if overleaf-track-changes
                       (format ",\"meta\": {\"tc\":\"%s\"}"
                               (xah-random-string 18))
                     "")
                   (1+ overleaf--doc-version) overleaf--doc-version (get-hash))
           (1+ overleaf--doc-version))
          (setq-local sequence-id (1+ sequence-id))
          (setq-local overleaf--doc-version (1+ overleaf--doc-version))
          (setq overleaf--edit-queue '())
          (setq-local buffer-read-only nil))))))


(defvar-local overleaf--before-change "")
(defvar-local overleaf--before-change-begin -1)

(defun on-change (begin end length)
  ;; (message "after %i %i %s %S" begin end length overleaf--last-change-type)
  (let ((overleaf--buffer (current-buffer)))
    (unless overleaf--is-overleaf-change
      (let ((new (buffer-substring-no-properties begin end)))
        ;; (message "%s %s" (json-encode-string overleaf--before-change) (json-encode-string new))
        (unless (and (equal new overleaf--before-change))
          (let ((empty-before (equal overleaf--before-change ""))
                (empty-after (equal new "")))

            (cond
             (empty-before
              (let ((begin-matches (= begin overleaf--last-change-end))
                    (end-matches (= end overleaf--last-change-begin)))
                ;; (message "insert \"%s\"" new)

                (if (or (not overleaf--last-change-type) (eq overleaf--last-change-type :d) (not (or begin-matches end-matches)))
                    (progn
                      (setq overleaf--last-change-type :i)
                      (setq overleaf--last-change-end end)
                      (setq overleaf--last-change-begin begin))

                  (setq overleaf--last-change-type :i)

                  ;; extend
                  (cond
                   (begin-matches
                    (setq overleaf--last-change-end end))
                   (end-matches
                    (setq overleaf--last-change-begin begin))))))
             (empty-after
              ;; (when (and overleaf--last-change-type (not (eq overleaf--last-change-type :d)))
              ;;   (message "NOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"))

              ;; (message "del" )

              (unless overleaf--last-change-type
                (setq-local overleaf--last-change-begin overleaf--before-change-begin)
                (setq-local overleaf--last-change-end end))

              (setq overleaf--last-change-type :d)
              (if (> overleaf--last-change-begin  overleaf--before-change-begin)
                  (progn
                    (setq overleaf--last-change-begin overleaf--before-change-begin)
                    (setq-local overleaf--deletion-buffer (concat overleaf--before-change overleaf--deletion-buffer)))
                (setq-local overleaf--deletion-buffer (concat overleaf--deletion-buffer overleaf--before-change))
                (setq overleaf--last-change-end end))
              (setq-local overleaf--last-change-begin overleaf--before-change-begin))
             (t
              (let ((overleaf--is-overleaf-change t))
                (delete-region begin end)
                (save-excursion
                  (goto-char overleaf--before-change-begin)
                  (insert overleaf--before-change)))
              (let ((overleaf--is-overleaf-change nil))
                (delete-region overleaf--before-change-begin overleaf--before-change-end))

              (setq-local overleaf--last-change-begin overleaf--before-change-begin)
              (setq-local overleaf--last-change-end overleaf--before-change-end)
              (setq-local overleaf--last-change-type :d)
              (setq-local overleaf--deletion-buffer overleaf--before-change)
              (queue-change)
              (overleaf--flush-edit-queue overleaf--buffer)
              (goto-char overleaf--before-change-begin)
              (setq-local overleaf--last-change-begin begin)
              (setq-local overleaf--last-change-end end)
              (setq-local overleaf--last-change-type :i)
              (insert new)
              (queue-change)
              (overleaf--flush-edit-queue overleaf--buffer)))))))))

(defvar-local overleaf--before-change-end -1)

(defun before-change (begin end)
  (unless overleaf--is-overleaf-change
    (let ((overleaf--buffer (current-buffer)))
      (setq-local overleaf--before-change (buffer-substring-no-properties begin end))
      (setq-local overleaf--before-change-begin begin)
      (setq-local overleaf--before-change-end end)
      (when
          (or
           (not (or (= begin overleaf--last-change-begin) (= begin overleaf--last-change-end) (= end overleaf--last-change-begin) (= end overleaf--last-change-end)))
           (and (= end begin) (or (eq overleaf--last-change-type :d)))
           (and (eq overleaf--last-change-type :i) (or (> end begin) (> (length overleaf--before-change) 0))))
        ;; (message "QUEUE %i %i %i %i %S" begin end overleaf--last-change-begin overleaf--last-change-end overleaf--last-change-type)
        (overleaf--flush-edit-queue (current-buffer))))))




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


(defun overleaf--update-modeline ()
  "Update the modeline string to reflect the current connection status."
  (setq-local overleaf--mode-line
              (concat
               "(O: "
               (if (websocket-openp overleaf--websocket)
                   (concat "["
                           (if (= overleaf--doc-version -1)
                               "⟲"
                             "✓")
                           (format ", %i" (length send-message-queue))
                           (if overleaf-track-changes
                               ", t"
                             "")
                           "]")
                 "[ ]")
               ")"))
  (force-mode-line-update t))

(defun overleaf--init ()
  "Set up the `overleaf-connection-mode`.

- Add the mode line status to the current mode line string.
- Turn off `inhibit-notification-hooks` as this prevents detecting changes
  to sync to overleaf."

  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'overleaf--mode-line global-mode-string)
    (setq global-mode-string (append global-mode-string
                                     '(overleaf--mode-line))))

  (overleaf--update-modeline)
  (setq inhibit-modification-hooks nil))

(defun overleaf-toggle-track-changes ()
  "Toggle tracking changes on overleaf."
  (interactive)
  (setq-local overleaf-track-changes (not overleaf-track-changes))
  (overleaf--write-buffer-variables)
  (overleaf--update-modeline))


(defmacro overleaf--key (key function)
  "Define a mapping of KEY to FUNCTION with the appropriate prefix."
  `(cons (kbd ,(concat overleaf-keymap-prefix " " key))  #',function))



;;;###autoload
(define-minor-mode overleaf-connection-mode
  "Toggle Overleaf Connection mode.
Interactively with no argument, this command toggles the mode."

  :init-value nil
  :ligther " Overleaf"
  :keymap
  (list
   (overleaf--key "c" connect-over)
   (overleaf--key "d" disconnect)
   (overleaf--key "t" overleaf-toggle-track-changes))

  (overleaf--init))
