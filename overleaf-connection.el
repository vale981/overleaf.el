;;; overleaf-connection.el --- Sync and track changes live with overleaf. -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2025 Valentin Boettcher


;; Author: Valentin Boettcher
;; Maintainer: Valentin Boettcher <overleaf-connection at protagon.space>
;; Created: March 18, 2025
;; URL: https://github.com/vale981/overleaf-connection.el
;; Package-Requires: ((plz "0.9") (websocket "1.15"))
;; Version: 0.0.2
;; Keywords: latex, overleaf

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Provides a minor mode that allows to sync the changes of a buffer
;; to an overleaf instance (https://github.com/overleaf/overleaf).


(require 'websocket)
(require 'plz)

;;; Code:
(eval-and-compile
  (defcustom overleaf-keymap-prefix "C-c C-o"
    "The prefix for dotcrafter-mode key bindings."
    :type 'string
    :group 'overleaf-connection-mode))

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

(defcustom overleaf-debug nil
  "Whether to log debug messages."
  :type 'boolean
  :group 'overleaf-connection-mode)

(defvar-local overleaf-auto-save nil
  "Whether to auto-save the buffer each time a change is synced.")

(defvar-local overleaf-project-id nil
  "The overleaf project id.

When having a project opened in the browser the URL should read
\"https://[overleaf-domain]/project/[project-id]\".")

(defvar-local overleaf-document-id nil
  "The overleaf document id as a string.

The id is most easily obtained by downloading the file that is to be
edited from the overleaf interface.   The download URL will then be of the form
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

(defvar-local overleaf--flush-edit-queue-timer nil
  "A timer that flushes the edit queue.

See `overleaf--edit-queue'.")

(defvar-local overleaf--message-timer nil
  "A timer that sends edits from the message queue if it isn't empty.

See `overleaf--message-queue'.")

(defvar-local overleaf--edit-queue '()
  "A list of edits that can be send in one go.")

(defvar-local overleaf--send-message-queue '()
  "A list of messages containing edits that have yet to be synced to overleaf.

See `overleaf--message-timer'.")

(defvar-local overleaf--edit-in-flight nil
  "If non-nil, we still await the acknowledgment from overleaf.")

(defvar-local overleaf--doc-version -1
  "Current version of the document.")

(defvar-local overleaf--mode-line ""
  "Contents of the mode-line indicator.")

(defvar overleaf--ws-url->buffer-table (make-hash-table :test #'equal)
  "A hash table associating web-sockets to buffers.")

(defvar overleaf--buffer nil
  "The current overleaf buffer (used in lexical binding).")

(defun overleaf--debug (format-string &rest args)
  "Print a debug message with format string FORMAT-STRING and arguments ARGS."
  (when overleaf-debug
    (if overleaf--buffer
        (with-current-buffer overleaf--buffer
          (with-current-buffer (get-buffer-create (format "*overleaf-%s*" overleaf-document-id))
            (setq buffer-read-only nil)
            (goto-char (point-max))
            (insert (apply #'format format-string args))
            (insert "\n")
            (setq buffer-read-only t)))
      (apply #'warn format-string args))))

(defun overleaf--connected-p ()
  "Return t if the buffer is connected to overleaf."
  (and overleaf--websocket
       (websocket-openp overleaf--websocket)
       (>= overleaf--doc-version 0)))

(defun overleaf--write-buffer-variables ()
  "Write the current buffer-local variables to the buffer."
  (when (overleaf--connected-p)
    (save-excursion
      (let ((overleaf--is-overleaf-change nil)
            (track-changes overleaf-track-changes))
        (setq-local overleaf-track-changes nil)
        (add-file-local-variable 'overleaf-document-id overleaf-document-id)
        (add-file-local-variable 'overleaf-project-id overleaf-project-id)
        (add-file-local-variable 'overleaf-track-changes track-changes)
        (add-file-local-variable 'overleaf-auto-save overleaf-auto-save)
        (overleaf--flush-edit-queue (current-buffer))
        (setq-local overleaf-track-changes track-changes)))))

(defun overleaf--parse-message (ws message)
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
               ("connectionRejected"
                (warn "Overleaf connection error: %S" (plist-get message :args))
                (overleaf-disconnect))
               ("otUpdateError"
                (overleaf--debug "-------- Update ERROR")
                (warn "Overleaf update error: %S" (plist-get message :args)))
               ("joinProjectResponse"
                (websocket-send-text ws (format "5:2+::{\"name\":\"joinDoc\",\"args\":[\"%s\",{\"encodeRanges\":true}]}" overleaf-document-id)))
               ("serverPing"
                (overleaf--debug "Received Ping -> PONG")
                (let ((res (concat id ":::" (json-encode `(:name "clientPong" :args ,(plist-get message :args))))))
                  (websocket-send-text ws res)))
               ("otUpdateApplied"
                (let ((version (plist-get (car (plist-get message :args)) :v))
                      (overleaf--is-overleaf-change t))
                  (overleaf--debug "Got update with version %s (buffer version %s)" version overleaf--doc-version)
                  (when (eq overleaf--edit-in-flight version)
                    (overleaf--debug "BINGO, we've been waiting for this.")
                    (setq overleaf--edit-in-flight nil)
                    (when (and (<= (length overleaf--send-message-queue) 1) overleaf-auto-save)
                      (overleaf--save-buffer)))

                  (when (> version overleaf--doc-version)
                    (setq overleaf--doc-version version)
                    (undo-boundary)
                    (save-excursion
                      (dolist (op (plist-get (car  (plist-get message :args)) :op))
                        (goto-char (1+ (plist-get op :p)))
                        (when-let* ((insert (plist-get op :i)))
                          (insert (overleaf--decode-utf8 insert)))
                        (when-let* ((delete (plist-get op :d)))
                          (re-search-forward (regexp-quote delete) nil t)
                          (replace-match ""))))
                    (when overleaf-auto-save
                      (overleaf--save-buffer))
                    (setq buffer-undo-list (memq nil buffer-undo-list)))))))))))))

(defun overleaf--save-buffer ()
  "Safely save the buffer."
  (let ((overleaf--is-overleaf-change t))
    (setq-local buffer-read-only t)
    (save-buffer)
    (setq-local buffer-read-only nil)))

(defun overleaf--decode-utf8 (str)
  "Decode the weird overleaf utf8 decoding in STR.

This calls out to node.js for now."
  (with-temp-buffer
    (call-process "node" nil t nil (concat (file-name-directory (symbol-file 'overleaf--decode-utf8)) "decode.js") str)
    (buffer-string)))

(defun overleaf-disconnect ()
  "Disconnect from overleaf."
  (interactive)
  (when overleaf--websocket
    (message "Overleaf Disconnecting")
    (setq-local overleaf--force-close t)
    (setq-local overleaf--edit-queue '())
    (setq-local overleaf--send-message-queue '())
    (websocket-close overleaf--websocket)
    (when overleaf-auto-save
      (overleaf--save-buffer))
    (setq-local overleaf--force-close nil)
    (remhash overleaf--websocket overleaf--ws-url->buffer-table)))

(defun overleaf--on-close (ws)
  "Handle the closure of the websocket WS."
  (let ((overleaf--buffer
         (gethash (websocket-url ws) overleaf--ws-url->buffer-table)))
    (with-current-buffer overleaf--buffer
      (when overleaf--websocket
        (message "Overleaf websocket for document %s closed." overleaf-document-id)
        (setq-local buffer-read-only nil)
        (cancel-timer overleaf--message-timer)
        (cancel-timer overleaf--flush-edit-queue-timer)
        (remhash (websocket-url ws) overleaf--ws-url->buffer-table)
        (setq-local overleaf--websocket nil)
        (overleaf--update-modeline)
        (unless overleaf--force-close
          (with-current-buffer overleaf--buffer
            (setq buffer-read-only t)
            (sleep-for .1)
            (setq overleaf--websocket nil)
            (overleaf-connect)))))))

(defun overeleaf--on-message (ws frame)
  "Handle a message received from websocket WS with contents FRAME."
  (let ((overleaf--buffer
         (gethash (websocket-url ws) overleaf--ws-url->buffer-table)))
    (overleaf--debug "Got message %S" frame)
    (overleaf--parse-message ws (websocket-frame-text frame))))

(defun overleaf--on-open (_websocket)
  "Handle the open even of the web-socket _WEBSOCKET."
  (let ((overleaf--buffer
         (gethash (websocket-url _websocket) overleaf--ws-url->buffer-table)))

    (with-current-buffer overleaf--buffer
      (overleaf--update-modeline)

      (add-hook 'after-change-functions #'overleaf--after-change-function nil :local)
      (add-hook 'before-change-functions #'overleaf--before-change-function nil :local)
      (add-hook 'kill-buffer-hook #'overleaf-disconnect nil :local)

      (setq-local
       overleaf--message-timer
       (progn
         (when overleaf--message-timer
           (cancel-timer overleaf--message-timer))
         (run-at-time t overleaf-message-interval #'overleaf--send-queued-message overleaf--buffer)))

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

(defun overleaf-toggle-track-changes ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-track-changes (not overleaf-track-changes))
  (overleaf--write-buffer-variables)
  (overleaf--update-modeline))

(defun overleaf-toggle-auto-save ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-auto-save (not overleaf-auto-save))
  (overleaf--write-buffer-variables))

(defun overleaf-connect ()
  "Connect to overleaf.
Requires `overleaf-cookies' to be set.  Prompts for the
`overleaf-project-id' and `overleaf-document-id` and saves them in the
file."
  (interactive)

  (overleaf-connection-mode t)
  (overleaf-disconnect)
  (if overleaf-cookies
      (let ((overleaf--buffer (current-buffer)))
        (with-current-buffer overleaf--buffer
          (setq overleaf-project-id
                (or overleaf-project-id
                    (read-from-minibuffer "Overleaf project id: ")))
          (setq overleaf-document-id
                (or overleaf-document-id
                    (read-from-minibuffer "Overleaf document id: ")))
          (let* ((cookies (overleaf--get-cokies))
                 (ws-id
                  (car (string-split
                        (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" overleaf-url overleaf-project-id)
                          :headers `(("Cookie" . ,cookies))) ":"))))

            (overleaf--debug "Connecting %s %s" overleaf-project-id overleaf-document-id)

            (setq-local overleaf--last-change-type nil)
            (setq-local overleaf--deletion-buffer "")
            (setq-local overleaf--edit-queue '())
            (setq-local overleaf--send-message-queue '())
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
                           :on-message #'overeleaf--on-message
                           :on-close #'overleaf--on-close
                           :on-open #'overleaf--on-open
                           :custom-header-alist `(("Cookie" . ,cookies)))))
             overleaf--buffer
             overleaf--ws-url->buffer-table)
            (overleaf--update-modeline))))
    (error "Please set `overleaf-cookies`")))

(defun overleaf--random-string (&optional CountX)
  "Return a random string of length COUNTX.

Pilfered from
URL `http://xahlee.info/emacs/emacs/elisp_insert_random_number_string.html'
Version: 2024-04-03"
  (interactive )
  (let ((xcharset "abcdfghjkmnpqrstvwxyz23456789") xcount xvec)
    (setq xcount (length xcharset))
    (setq xvec (mapcar (lambda (_) (aref xcharset (random xcount))) (make-vector (if CountX CountX 5) 0)))
    (mapconcat 'char-to-string xvec)))

(defun overleaf--get-hash ()
  "Get the hash of the overleaf buffer."
  (with-current-buffer overleaf--buffer
    (save-restriction
      (widen)
      (let ((buff (buffer-string)))
        (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff))))))


(defun overleaf--escape-string (str)
  "Escape STR to send to overleaf."
  (json-encode str))

(defun overleaf--queue-edit (edit)
  "Add EDIT to the edit queue."
  (overleaf--debug "====> adding %s to queue" edit)
  (setq overleaf--edit-queue (nconc overleaf--edit-queue (list edit))))

(defun overleaf--queue-message (message version)
  "Queue edit MESSAGE leading to buffer version VERSION to be send to overleaf."
  (setq overleaf--send-message-queue (nconc overleaf--send-message-queue `((,message . ,version)))))

(defun overleaf--send-queued-message (&optional buffer)
  "Send a message from the edit message queue of BUFFER if there is no other edit in flight."
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (unless overleaf--edit-in-flight
        (with-current-buffer overleaf--buffer
          (when overleaf--send-message-queue
            (when-let* ((message (car overleaf--send-message-queue)))
              (setq-local overleaf--edit-in-flight (cdr message))
              (websocket-send-text overleaf--websocket (car message))
              (websocket-send-text
               overleaf--websocket
               (format
                "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                (current-line) (current-column) overleaf-document-id))
              (setq overleaf--send-message-queue (cdr overleaf--send-message-queue)))))))))

(defun overleaf--overleaf-queue-current-change (&optional buffer)
  "Queue the change to BUFFER currently being built."
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (with-current-buffer overleaf--buffer
        (when (and overleaf--last-change-type (websocket-openp overleaf--websocket))
          (overleaf--debug "======> %s %i %i %s" overleaf--last-change-type overleaf--last-change-begin (point) overleaf--deletion-buffer)
          (pcase overleaf--last-change-type
            (:d
             (overleaf--queue-edit
              `(:p ,(- overleaf--last-change-begin 1) :d ,overleaf--deletion-buffer)))
            (:i
             (overleaf--queue-edit
              `(:p ,(- overleaf--last-change-begin 1) :i ,(buffer-substring-no-properties overleaf--last-change-begin overleaf--last-change-end)))))
          (setq-local overleaf--last-change-type nil)
          (setq-local overleaf--last-change-begin -1)
          (setq-local overleaf--last-change-end -1)
          (setq-local overleaf--deletion-buffer ""))))))

(defun overleaf--flush-edit-queue (buffer)
  "Make an edit message and append it to the message queue of BUFFER."
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (overleaf--overleaf-queue-current-change)
        (when (and overleaf--websocket (websocket-openp overleaf--websocket) overleaf--edit-queue)
          (setq-local buffer-read-only t)
          (overleaf--debug "======> FLUSH %i" overleaf--doc-version)
          (overleaf--queue-message
           (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s%s,\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                   sequence-id
                   overleaf-document-id
                   overleaf-document-id
                   (json-encode (apply #'vector overleaf--edit-queue))
                   (if overleaf-track-changes
                       (format ",\"meta\": {\"tc\":\"%s\"}"
                               (overleaf--random-string 18))
                     "")
                   (1+ overleaf--doc-version) overleaf--doc-version (overleaf--get-hash))
           (1+ overleaf--doc-version))
          (setq-local sequence-id (1+ sequence-id))
          (setq-local overleaf--doc-version (1+ overleaf--doc-version))
          (setq overleaf--edit-queue '())
          (setq-local buffer-read-only nil))))))


;;; Change Detection
(defvar-local overleaf--before-change "")
(defvar-local overleaf--before-change-begin -1)
(defvar-local overleaf--before-change-end -1)
(defvar-local overleaf--last-change-type nil)
(defvar-local overleaf--last-change-begin 0)
(defvar-local overleaf--last-change-end 0)
(defvar-local overleaf--deletion-buffer "")

(defun overleaf--after-change-function (begin end length)
  "The after change hook that detects a change in region BEGIN - END of length LENGTH to be sent to overleaf."
  (let ((overleaf--buffer (current-buffer)))
    (overleaf--debug "after (%i %i) %i (%i %i) %S" begin end length overleaf--last-change-begin overleaf--last-change-end overleaf--last-change-type)
    (unless overleaf--is-overleaf-change
      (let ((new (buffer-substring-no-properties begin end)))
        ;; the before change hook tends to lie about the end of the region
        (setq overleaf--before-change (substring overleaf--before-change 0 length))
        (setq overleaf--before-change-end (+ overleaf--before-change-begin length))

        (overleaf--debug "change %s -> %s" (json-encode-string overleaf--before-change) (json-encode-string new))
        (unless (and (equal new overleaf--before-change))
          (let ((empty-before (equal overleaf--before-change ""))
                (empty-after (equal new "")))

            (cond
             (empty-before
              (let ((begin-matches (= begin overleaf--last-change-end))
                    (end-matches (= begin overleaf--last-change-begin)))
                (overleaf--debug "insert \"%s\" %S" new overleaf--last-change-begin)

                (if (or (not overleaf--last-change-type) (eq overleaf--last-change-type :d) (not (or begin-matches end-matches)))
                    (progn
                      (overleaf--debug "looks like a new insert!")

                      (setq overleaf--last-change-type :i)
                      (setq overleaf--last-change-end end)
                      (setq overleaf--last-change-begin begin))
                  (setq overleaf--last-change-type :i)

                  ;; extend
                  (cond
                   (begin-matches
                    (overleaf--debug "Extending right to %s" end)
                    (setq overleaf--last-change-end end))
                   (end-matches
                    (overleaf--debug "Extending left to %s" end)
                    (setq overleaf--last-change-begin begin)
                    (setq overleaf--last-change-end (+ overleaf--last-change-end (- end begin))))))))
             (empty-after
              (overleaf--debug "delete")

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
              (overleaf--debug "====> Complicated Change")
              (overleaf--debug "====> Restored previous version... replaying")
              (overleaf--debug "====> Deleting %s" (buffer-substring-no-properties begin end))
              (delete-region begin end)
              (setq-local overleaf--last-change-begin overleaf--before-change-begin)
              (setq-local overleaf--last-change-end (+ overleaf--before-change-begin length))
              (setq-local overleaf--last-change-type :d)
              (setq-local overleaf--deletion-buffer overleaf--before-change)
              (overleaf--debug "====> Claiming to have deleted %s" overleaf--before-change)
              (overleaf--debug "====> buffer is now %s" (buffer-substring-no-properties begin end))

              (overleaf--overleaf-queue-current-change)
              (overleaf--flush-edit-queue overleaf--buffer)
              (goto-char overleaf--before-change-begin)
              (setq-local overleaf--last-change-begin begin)
              (setq-local overleaf--last-change-end end)
              (setq-local overleaf--last-change-type :i)
              (overleaf--debug "====> Inserting %s" new)
              (insert new)
              (overleaf--overleaf-queue-current-change)
              (overleaf--flush-edit-queue overleaf--buffer)))))))))


(defun overleaf--before-change-function (begin end)
  "Change hook called to signal an impending change between BEGIN and END.

Mainly used to detect switchover between deletion and insertion."
  (unless overleaf--is-overleaf-change
    (let ((overleaf--buffer (current-buffer)))
      (overleaf--debug "before (%i %i) (%i %i)" begin end overleaf--last-change-begin overleaf--last-change-end)
      (setq-local overleaf--before-change (buffer-substring-no-properties begin end))
      (setq-local overleaf--before-change-begin begin)
      (setq-local overleaf--before-change-end end)
      (when
          (or
           (not (or (= begin overleaf--last-change-begin)
                    (= begin overleaf--last-change-end)
                    (= end overleaf--last-change-begin)
                    (= end overleaf--last-change-end)))
           (and (= end begin) (or (eq overleaf--last-change-type :d)))
           (and (eq overleaf--last-change-type :i) (or (> end begin) (> (length overleaf--before-change) 0)))
           ;; (or (> (max (- end begin) (- overleaf--last-change-end overleaf--last-change-begin))
           ;;        10))
           )
        (overleaf--debug "Edit type switchover --> flushing edit queue %s" overleaf--edit-queue)
        (overleaf--overleaf-queue-current-change)
        (overleaf--flush-edit-queue (current-buffer))))))

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
                           (format ", %i" (length overleaf--send-message-queue))
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
   (overleaf--key "c" overleaf-connect)
   (overleaf--key "d" overleaf-disconnect)
   (overleaf--key "t" overleaf-toggle-track-changes)
   (overleaf--key "s" overleaf-toggle-auto-save))

  (if overleaf-connection-mode
      (overleaf--init)
    (setq-local overleaf--mode-line "")
    (force-mode-line-update t)
    (overleaf-disconnect)))


(provide 'overleaf-connection)

;;; overleaf-connection.el ends here
