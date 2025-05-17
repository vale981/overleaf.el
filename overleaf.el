;;; overleaf.el --- Sync and track changes live with overleaf -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2025 Valentin Boettcher


;; Author: Valentin Boettcher
;; Maintainer: Valentin Boettcher <overleaf at protagon.space>
;; Created: March 18, 2025
;; URL: https://github.com/vale981/overleaf.el
;; Package-Requires: ((emacs "29.4") (plz "0.9") (websocket "1.15") (webdriver "0.1"))
;; Version: 1.1.0
;; Keywords: hypermedia, tex, comm
;; SPDX-License-Identifier: GPL-3.0-or-later
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

(require 'webdriver)
(require 'webdriver-firefox)
(require 'websocket)
(require 'plz)

;;; Code:

;;;; Variables

(eval-and-compile
  (defcustom overleaf-keymap-prefix "C-c C-o"
    "The prefix for dotcrafter-mode key bindings."
    :type 'string
    :group 'overleaf-mode))

(defvar overleaf-cookies nil
  "The overleaf session cookies.

Can a string or function that returns a string containing the overleaf
authentication cookies.

For example the variable can be bound to a function that loads the
cookies from a gpg encrypted file.  See
`overleaf-read-cookies-from-file'.

The cookies are most easily obtained from the developer tools in the
browser.")


;;;###autoload
(defun overleaf-read-cookies-from-file (file)
  "Return a cookie saving function to load the cookie-string from FILE.
To be used with `overleaf-cookies'."
  (lambda ()
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (string-trim (buffer-string)))))

(defvar overleaf-save-cookies (lambda (cookies)
                                (setq overleaf-cookies cookies))
  "A function (lambda) that stores the session cookies.
The function receives a string containing the session cookies and stores
in a way that `overleaf-cookies' can access it.  The default
implementation simply sets `overleaf-cookies' to the string value.
Another possibility is to store them into a gpg encrypted file.  See
`overleaf-save-cookies-to-file'.")

;;;###autoload
(defun overleaf-save-cookies-to-file (file)
  "Return a cookie saving function to save the cookie-string to FILE.
To be used with `overleaf-save-cookies'."
  (lambda (cookies)
    (with-temp-file file
      (insert cookies))))

(defcustom overleaf-url "https://www.overleaf.com"
  "The url of the overleaf server."
  :type 'string
  :group 'overleaf-mode)

(defcustom overleaf-flush-interval .5
  "The idle-timer delay to flush the edit queue."
  :type 'float
  :group 'overleaf-mode)

(defcustom overleaf-message-interval .5
  "The interval for the timer that syncs changes to overleaf."
  :type 'float
  :group 'overleaf-mode)

(defcustom overleaf-debug nil
  "Whether to log debug messages."
  :type 'boolean
  :group 'overleaf-mode)

(defcustom overleaf-use-nerdfont nil
  "Whether to use nerd-font icons for the modeline."
  :type 'boolean
  :group 'overleaf-mode)

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
  "Is set to t if the current change in the buffer comes from overleaf.
Used to inhibit the change detection.")

(defvar-local overleaf--force-close nil
  "If t the connection will not be reestablished upon disconnection.")

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

(defvar-local overleaf--sequence-id -1
  "An internal counter for the websocket communication.")

(defvar-local overleaf--mode-line ""
  "Contents of the mode-line indicator.")

;; Do not ignore :propertize forms.  See variable `mode-line-format'.
(put 'overleaf--mode-line 'risky-local-variable t)

(defvar overleaf--ws-url->buffer-table (make-hash-table :test #'equal)
  "A hash table associating web-sockets to buffers.")

(defvar overleaf--buffer nil
  "The current overleaf buffer (used in lexical binding).")

(defvar-local overleaf--buffer-before-edit-queue ""
  "The contents of the buffer before any edits were queued.")

(defvar-local overleaf--last-good-state nil
  "The last received overleaf update.")

(defvar-local overleaf--history nil
  "An list that relates version numbers to the buffer text at that version.
Should only contain known-good states.  Is limited to length
`overleaf-history-buffer-length'.")

(defvar-local overleaf-history-buffer-length 50
  "How many past versions of the buffer to keep in memory.")

(defvar-local overleaf--recent-updates nil
  "An alist of recent updates received from overleaf.

An alist that contains the `overleaf-update-buffer-length'
recent updates.  It has elements of the form `((from-version
. (to-version . update)) ...)'.")

(defvar-local overleaf-update-buffer-length 50
  "How many past updates of the buffer to keep in memory.")

(defvar overleaf--current-cookies nil
  "The current cookies so we don't have to read them every time.")

(defvar-local overleaf--receiving nil
  "When t we are currently in the process of receiving and processing an update.")

(easy-menu-define overleaf-menu nil "Overleaf"
  '("Overleaf"
    ["Connect" overleaf-connect
     :help "Connect to overleaf"]
    ["Disconnect" overleaf-disconnect
     :help "Disconnect from overleaf"]
    ["Toggle auto save" overleaf-toggle-auto-save
     :help "Toggle auto-save on overleaf"]
    ["Toggle track changes" overleaf-toggle-track-changes
     :help "Toggle track-changes on overleaf"]
    ["Browse project" overleaf-browse-project
     :help "Browse project with `browse-url'"
     :active overleaf-project-id]))

(defvar overleaf--menu-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] overleaf-menu)
    map))

(cl-defstruct overleaf--queued-message
  "A container that holds a queued message."
  edits
  doc-version
  hash
  track-changes
  buffer-before
  buffer)

(cl-defstruct overleaf--update
  "An update received from or sent to overleaf.

EDITS are a list of edit operations (inserts and deletes).
FROM-VERSION is the buffer version being edited.
TO-VERSION is the buffer version we hope to reach.
HASH is a hash obtained from `overleaf--get-hash' after the
edit is performed.
BUFFER is the buffer value after applying the update."
  edits
  from-version
  to-version
  hash
  buffer)


;;;; Macros
(defmacro overleaf--warn (&rest args)
  "Print a warning message passing ARGS on to `display-warning'."
  `(display-warning 'overleaf (format ,@args)))

(defmacro overleaf--message (string &rest args)
  "Print a message with format string STRING and arguments ARGS."
  `(message  ,(concat "Overleaf: " string) ,@args))

;;;; Communication

(defun overleaf--get-full-cookies ()
  "Load the association list domain<->cookies."
  (if overleaf--current-cookies
      overleaf--current-cookies
    (condition-case err
        (setq overleaf--current-cookies
              (read
               (if (or (functionp overleaf-cookies)
                       (fboundp 'overleaf-cookies))
                   (funcall overleaf-cookies)
                 overleaf-cookies)))
      (error
       (overleaf--warn "Error while loading cookies: %s" (error-message-string err))
       nil))))

(defun overleaf--get-cookies ()
  "Load the cookies from `overleaf-cookies'."
  (if-let
      ((cookies
        (alist-get (overleaf--cookie-domain)
                   (overleaf--get-full-cookies)
                   nil nil #'string=))
       (now (time-convert nil 'integer))) ; Current unix time in seconds.
      (pcase-let ((`(,value ,validity) cookies))
        (if (or (not validity) (< now validity))
            value
          (user-error "Cookies for %s are expired.  Please refresh them using `overleaf-authenticate' or manually"
                      (overleaf--cookie-domain))))
    (user-error "Cookies for %s are not set.  Please set them using `overleaf-get-cookies' or manually"
                (overleaf--cookie-domain))))

(defun overleaf--connected-p ()
  "Return t if the buffer is connected to overleaf."
  (and overleaf--websocket
       (websocket-openp overleaf--websocket)
       (>= overleaf--doc-version 0)))

(defun overleaf--on-open (websocket)
  "Handle the open even of the web-socket WEBSOCKET."
  (let ((overleaf--buffer
         (gethash (websocket-url websocket) overleaf--ws-url->buffer-table)))

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

(defun overleaf--on-message (ws frame)
  "Handle a message received from websocket WS with contents FRAME."
  (let ((overleaf--buffer
         (gethash (websocket-url ws) overleaf--ws-url->buffer-table)))
    (overleaf--debug "Got message %S" frame)
    (overleaf--parse-message ws (websocket-frame-text frame))))

(defun overleaf--on-close (ws)
  "Handle the closure of the websocket WS."
  (let ((overleaf--buffer
         (gethash (websocket-url ws) overleaf--ws-url->buffer-table)))
    (with-current-buffer overleaf--buffer
      (when overleaf--websocket
        (overleaf--message "Websocket for document %s closed." overleaf-document-id)
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
             (when doc
               (let ((point (point)))
                 (setq-local buffer-read-only nil)
                 (erase-buffer)
                 (insert (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n")))
                 (overleaf--write-buffer-variables)
                 (goto-char point)
                 (setq buffer-undo-list nil))
               (overleaf--set-version version)
               (overleaf--push-to-history version))))
         (overleaf--update-modeline))
        ("5"
         (when message-raw
           (when-let* ((message (json-parse-string message-raw :object-type 'plist :array-type 'list))
                       (name (plist-get message :name)))

             (pcase name
               ("connectionRejected"
                (overleaf--warn "Connection error: %S" (plist-get message :args))
                (overleaf-disconnect))
               ("otUpdateError"
                (overleaf--debug "-------- Update ERROR")
                (overleaf--warn "Update error %S" (car (plist-get message :args)))
                (overleaf-connect))
               ("joinProjectResponse"
                (websocket-send-text ws (format "5:2+::{\"name\":\"joinDoc\",\"args\":[\"%s\",{\"encodeRanges\":true}]}" overleaf-document-id)))
               ("serverPing"
                (overleaf--debug "Received Ping -> PONG")
                (let ((res (concat id ":::" (json-encode `(:name "clientPong" :args ,(plist-get message :args))))))
                  (websocket-send-text ws res)))
               ("otUpdateApplied"
                (setq overleaf--receiving t)
                (let ((last-version (plist-get (car (plist-get message :args)) :lastV))
                      (version (plist-get (car (plist-get message :args)) :v))
                      (hash (plist-get (car (plist-get message :args)) :hash))
                      (overleaf--is-overleaf-change t)
                      (edits (when (plist-get message :args) (plist-get (car  (plist-get message :args)) :op))))
                  (overleaf--debug "%S Got update with version %s->%s (buffer version %s) %S" (buffer-name) last-version version overleaf--doc-version message)
                  (overleaf--apply-changes edits version last-version hash))
                (setq overleaf--receiving nil)))
             (overleaf--send-queued-message))))))))

(defun overleaf--get-hash ()
  "Get the hash of the overleaf buffer."
  (with-current-buffer overleaf--buffer
    (save-restriction
      (widen)
      (let ((buff (buffer-string)))
        (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff))))))

(defun overleaf--push-to-history (version &optional buffer-string)
  "Push the buffer to the version history.
Push the contents of the buffer or BUFFER-STRING of VERSION to the local
overleaf version history."
  (when (and version overleaf--buffer)
    (with-current-buffer overleaf--buffer
      (setq-local overleaf--history
                  (overleaf--splice-into overleaf--history version
                                         (or buffer-string (buffer-string)) t))
      (setq-local overleaf--history
                  (overleaf--truncate
                   overleaf--history
                   overleaf-history-buffer-length))
      (when overleaf-auto-save
        (save-buffer)))))

(defun overleaf--push-to-recent-updates (update)
  "Splice the UPDATE of type `overleaf--update' into `overleaf--recent-updates'."
  (let ((from-version (overleaf--update-from-version update))
        (to-version (overleaf--update-to-version update)))
    (when (and from-version to-version overleaf--buffer)
      (with-current-buffer overleaf--buffer
        (setq-local overleaf--recent-updates
                    (overleaf--splice-into
                     overleaf--recent-updates from-version (cons to-version update)))
        (setq-local overleaf--recent-updates
                    (overleaf--truncate
                     overleaf--recent-updates overleaf-update-buffer-length))))))

(defun overleaf--queue-message (message)
  "Queue edit MESSAGE leading to buffer version VERSION to be send to overleaf."
  (setq overleaf--send-message-queue (nconc overleaf--send-message-queue (list message))))

(defun overleaf--send-queued-message (&optional buffer)
  "Send the next message in the message queue to overleaf.

Send a message from the edit message queue of BUFFER if there is no
other edit in flight."
  (let ((overleaf--buffer (or buffer overleaf--buffer)))
    (when overleaf--buffer
      (unless (or overleaf--edit-in-flight overleaf--receiving)
        (with-current-buffer overleaf--buffer
          (when overleaf--send-message-queue
            (when-let* ((message (car overleaf--send-message-queue))
                        (current-version (overleaf--queued-message-doc-version message))
                        (next-version (1+ current-version)))
              (websocket-send-text
               overleaf--websocket
               (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s%s,\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                       overleaf--sequence-id
                       overleaf-document-id
                       overleaf-document-id
                       (json-encode (apply #'vector (overleaf--queued-message-edits message)))
                       (if (overleaf--queued-message-track-changes message)
                           (format ",\"meta\": {\"tc\":\"%s\"}"
                                   (overleaf--random-string 18))
                         "")
                       next-version
                       current-version
                       (overleaf--queued-message-hash message)))
              (websocket-send-text
               overleaf--websocket
               (format
                "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                (line-number-at-pos) (current-column) overleaf-document-id))

              (setq-local overleaf--sequence-id (1+ overleaf--sequence-id))

              (setq-local overleaf--edit-in-flight
                          (make-overleaf--update
                           :from-version current-version
                           :to-version next-version
                           :edits (overleaf--queued-message-edits message)
                           :hash (overleaf--queued-message-hash message)
                           :buffer (overleaf--queued-message-buffer message)))
              (overleaf--debug "send %S %i %i" (buffer-name) current-version next-version)
              (setq overleaf--send-message-queue (cdr overleaf--send-message-queue)))))))))

(defun overleaf--apply-changes-internal (edits)
  "Parse the edit list EDITS and apply them to the buffer.

Returns the edits as applied.  This is required because deletions might
no longer be possible, or will occur at a different location."
  (let ((overleaf--is-overleaf-change t))
    (save-excursion
      (remq nil
            (mapcar
             (lambda (op)
               (goto-char (1+ (plist-get op :p)))
               (if-let* ((insert (plist-get op :i)))
                   (progn
                     (insert insert)
                     (setq buffer-undo-list (memq nil buffer-undo-list))
                     op)
                 (if-let* ((delete (plist-get op :d)))
                     (when (re-search-forward (regexp-quote delete) nil)
                       (replace-match "")
                       (overleaf--debug "applied delete %S %S" op `(:p ,(1- (point)) :d ,delete))
                       (setq buffer-undo-list (memq nil buffer-undo-list))
                       `(:p ,(1- (point)) :d ,delete)))))
             edits)))))

(cl-defun overleaf--apply-changes (edits version last-version hash)
  "Apply change EDITS  from version LAST-VERSION to VERSION to have the hash HASH.

If there are some updates to the buffer that haven't yet been
acknowledged by overleaf or even haven't yet been sent we have to replay
them on top of the changes received from overleaf in the meantime."
  (with-current-buffer overleaf--buffer
    (overleaf--flush-edit-queue overleaf--buffer)
    (let ((point (point)))
      (save-excursion
        (let ((overleaf--is-overleaf-change t))
          (if (and overleaf--edit-in-flight (not last-version))
              (progn
                (overleaf--debug  "%s BINGO, we've been waiting for this. %S %S" (buffer-name) overleaf--doc-version version)
                (setf (overleaf--update-to-version overleaf--edit-in-flight) version)
                (overleaf--push-to-recent-updates
                 overleaf--edit-in-flight)
                (overleaf--push-to-history
                 version
                 (when overleaf--send-message-queue
                   (overleaf--queued-message-buffer-before
                    (car overleaf--send-message-queue))))
                (setq overleaf--edit-in-flight nil))

            (overleaf--set-version version)
            (overleaf--push-to-recent-updates
             (make-overleaf--update :from-version (or last-version (1- version)) :to-version version :edits edits :hash hash)))

          (if (or overleaf--edit-in-flight overleaf--send-message-queue)
              (progn
                (when last-version
                  (if (alist-get last-version overleaf--history)
                      (progn
                        (overleaf--debug "%s replaying" (buffer-name))
                        (erase-buffer)
                        (insert (alist-get last-version overleaf--history))
                        (dolist (change overleaf--recent-updates)
                          (when (>= (car change) last-version)
                            (overleaf--debug "~~~ ----------> %S" change)
                            (overleaf--apply-changes-internal (overleaf--update-edits (cdr (cdr change))))))
                        (overleaf--push-to-history version)


                        (when overleaf--edit-in-flight
                          (overleaf--debug "----------> %S" overleaf--edit-in-flight)
                          (setf (overleaf--update-from-version overleaf--edit-in-flight) overleaf--doc-version)
                          (if (overleaf--apply-changes-internal (overleaf--update-edits overleaf--edit-in-flight))
                              (progn
                                (setf (overleaf--update-buffer overleaf--edit-in-flight) (buffer-string))
                                (overleaf--set-version (1+ overleaf--doc-version)))
                            (overleaf--debug "failed to apply")
                            (setq-local overleaf--edit-in-flight nil))))

                    (overleaf--warn "We haven't seen document version %i yet. Reconnecting" last-version)
                    (overleaf-connect)))

                (when (and overleaf--send-message-queue (or last-version (> version overleaf--doc-version)))
                  (let ((buffer-before-application nil))

                    (when (> version overleaf--doc-version)
                      (erase-buffer)
                      (insert (overleaf--queued-message-buffer-before (car overleaf--send-message-queue))))

                    (setq-local
                     overleaf--send-message-queue
                     (remq nil
                           (mapcar
                            (lambda (change)
                              (overleaf--debug "->>>>>>>>>>>>>>>>>>>>>>>>>>>> %S" change)
                              (setq buffer-before-application (buffer-string))
                              (if-let* ((edits
                                         (overleaf--apply-changes-internal
                                          (overleaf--queued-message-edits change))))
                                  (progn
                                    (setf (overleaf--queued-message-doc-version change) overleaf--doc-version)
                                    (setf (overleaf--queued-message-hash change) (overleaf--get-hash))
                                    (setf (overleaf--queued-message-edits change) edits)
                                    (setf (overleaf--queued-message-buffer-before change) buffer-before-application)
                                    (setf (overleaf--queued-message-buffer change) (buffer-string))
                                    (overleaf--set-version (1+ overleaf--doc-version))
                                    change)
                                (erase-buffer)
                                (insert buffer-before-application)
                                nil))
                            overleaf--send-message-queue))))))


            (overleaf--apply-changes-internal edits)
            (overleaf--set-version version)
            (overleaf--push-to-history version))


          (when last-version
            (when (and hash last-version
                       (not overleaf--edit-in-flight)
                       (not overleaf--send-message-queue)
                       (= (- version last-version) 1)
                       (not (string= (overleaf--get-hash) hash)))
              (overleaf--warn "Hash mismatch... reconnecting")
              (setq-local buffer-read-only t)
              (overleaf-connect)))))
      (goto-char point))))


;;;; Misc

(defun overleaf--set-version (vers)
  "Set the buffer version to VERS."
  (overleaf--debug "Setting buffer version to %s" vers)
  (setq-local overleaf--doc-version vers))

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

(defun overleaf--save-buffer ()
  "Safely save the buffer."
  (let ((overleaf--is-overleaf-change t))
    (setq-local buffer-read-only t)
    (save-buffer)
    (setq-local buffer-read-only nil)))

(defun overleaf--splice-into (list vers content &optional unique)
  "Splice a new element CONTENT into a an alist LIST sorted by VERS.
If unique is t the element with key VERS will be overwritten."
  (let ((head-vers (car (car list))))
    (if (or (not head-vers) (< vers head-vers))
        `((,vers . ,content) ,@list)
      (if (and unique (= vers head-vers))
          `((,vers . ,content) ,@(cdr list))
        `(,(car list) ,@(overleaf--splice-into (cdr list) vers content))))))

(defun overleaf--truncate (list max-length)
  "Remove the first elements from LIST to make it MAX-LENGTH long."
  (nthcdr (max 0 (- (length list) max-length)) list))

(defun overleaf--url ()
  "Return a sanitized version of the url without trailing slash."
  (string-trim (string-trim overleaf-url) "" "/"))

(defun overleaf--cookie-domain ()
  "Return the domain for which the cookies will be valid.
The value is computed from the current value of `overleaf-url'."
  (let ((domain-parts (string-split (overleaf--url) "\\.")))
    (string-join (last domain-parts 2) ".")))

(defun overleaf--decode-utf8 (string)
  "Decode the weird overleaf utf8 decoding in STRING."
  (decode-coding-string
   (mapconcat #'byte-to-string string) 'utf-8))

(defun overleaf--random-string (&optional CountX)
  "Return a random string of length COUNTX.

Pilfered from
URL `http://xahlee.info/emacs/emacs/elisp_insert_random_number_string.html'
Version: 2024-04-03"
  (let ((xcharset "abcdfghjkmnpqrstvwxyz23456789") xcount xvec)
    (setq xcount (length xcharset))
    (setq xvec (mapcar (lambda (_) (aref xcharset (random xcount))) (make-vector (if CountX CountX 5) 0)))
    (mapconcat #'char-to-string xvec)))

;;;; Logging
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

;;;; Webdriver
(defmacro overleaf--with-webdriver (&rest body)
  "Execute BODY if geckodriver is found and show an error message otherwise."
  `(if (not (executable-find "geckodriver"))
       (message-box "Please install geckodriver or set the cookies / document and project id manually.")
     ,@body))

(cl-defmacro overleaf--webdriver-wait-until-appears
    ((session xpath &optional (element-sym '_unused) (delay .1)) &rest body)
  "Wait until an element matching XPATH is found in SESSION.
The element is then bound to ELEMENT-SYM and the BODY is executed."
  (let ((not-found (gensym))
        (sel-var (gensym)))
    `(let ((,sel-var
            (make-instance 'webdriver-by
                           :strategy "xpath"
                           :selector ,xpath))
           (,not-found t))
       (while ,not-found
         (condition-case nil
             (let ((,element-sym
                    (webdriver-find-element ,session ,sel-var)))
               (setq ,not-found nil)
               ,@body)
           (webdriver-error
            (sleep-for ,delay)))))))


(defun overleaf--webdriver-set-cookies (session)
  "Set the cookies in the webdriver session SESSION."
  (let ((cookie-domain (overleaf--cookie-domain))
        (cookies (overleaf--get-cookies)))
    (when cookies
      (dolist (cookie (string-split cookies ";"))
        (pcase-let ((`(,name ,value) (string-split cookie "=")))
          (webdriver-add-cookie
           session
           `(:name ,(string-trim name) :value ,(string-trim value) :domain
                   ,cookie-domain)))))))


;;;; Change Detection

(defvar-local overleaf--before-change "")
(defvar-local overleaf--before-change-begin -1)
(defvar-local overleaf--before-change-end -1)
(defvar-local overleaf--last-change-type nil)
(defvar-local overleaf--last-change-begin 0)
(defvar-local overleaf--last-change-end 0)
(defvar-local overleaf--deletion-buffer "")

(defun overleaf--after-change-function (begin end length)
  "The after change hook that is called after the buffer changed.
The change is in the region BEGIN - END of length LENGTH to be sent to
overleaf."
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
              (let ((overleaf--is-overleaf-change t))
                (delete-region begin end))
              (setq-local overleaf--last-change-begin overleaf--before-change-begin)
              (setq-local overleaf--last-change-end (+ overleaf--before-change-begin length))
              (setq-local overleaf--last-change-type :d)
              (setq-local overleaf--deletion-buffer overleaf--before-change)
              (overleaf--debug "====> Claiming to have deleted %s" overleaf--before-change)
              (when (< end (point-max))
                (overleaf--debug "====> buffer is now %s" (buffer-substring-no-properties begin end)))

              (overleaf-queue-current-change)
              (overleaf--flush-edit-queue overleaf--buffer)
              (goto-char overleaf--before-change-begin)
              (setq-local overleaf--last-change-begin begin)
              (setq-local overleaf--last-change-end end)
              (setq-local overleaf--last-change-type :i)
              (overleaf--debug "====> Inserting %s" new)
              (let ((overleaf--is-overleaf-change t))
                (insert new))
              (overleaf-queue-current-change)
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
        (overleaf-queue-current-change)
        (overleaf--flush-edit-queue (current-buffer))))))

(defun overleaf--flush-edit-queue (buffer)
  "Make an edit message and append it to the message queue of BUFFER."
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (overleaf-queue-current-change)
        (when (and overleaf--websocket (websocket-openp overleaf--websocket) overleaf--edit-queue)
          (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
            (setq-local buffer-read-only t)
            (overleaf--debug "======> FLUSH %S %i" (buffer-name) overleaf--doc-version)
            (overleaf--queue-message
             (make-overleaf--queued-message
              :edits (mapcar #'copy-sequence overleaf--edit-queue)
              :doc-version overleaf--doc-version
              :hash (overleaf--get-hash)
              :track-changes overleaf-track-changes
              :buffer-before overleaf--buffer-before-edit-queue
              :buffer buf-string))
            (setq-local overleaf--buffer-before-edit-queue (concat buf-string))
            (overleaf--set-version (1+ overleaf--doc-version))
            (setq overleaf--edit-queue '())
            (setq-local buffer-read-only nil)))))))

(defun overleaf--queue-edit (edit)
  "Add EDIT to the edit queue."
  (overleaf--debug "====> adding %s to queue" edit)
  (setq overleaf--edit-queue (nconc overleaf--edit-queue (list edit))))


(defun overleaf-queue-current-change (&optional buffer)
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

;;;; Interface

;;;###autoload
(defun overleaf-toggle-track-changes ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-track-changes (not overleaf-track-changes))
  (overleaf--write-buffer-variables)
  (overleaf--update-modeline))

;;;###autoload
(defun overleaf-toggle-auto-save ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-auto-save (not overleaf-auto-save))
  (overleaf--write-buffer-variables))

;;;###autoload
(defun overleaf-browse-project ()
  "Browse the current project with `browse-url'.
`overleaf-url' and `overleaf-project-id' define the current project."
  (interactive)
  (unless overleaf-project-id
    (user-error "Variable `overleaf-project-id' is not set"))
  (browse-url (format "%s/project/%s" (overleaf--url) overleaf-project-id)))

;;;###autoload
(defun overleaf-authenticate (url)
  "Use selenium webdriver to log into overleaf URL and obtain the cookies.
After running this command, wait for the browser-window to pop up and
for the login page to load.  Note that if the cookies are still valid,
the login page may not be shown and this command terminates without user input.

Requires `geckodriver' (see
https://github.com/mozilla/geckodriver/releases) to be installed."
  (interactive
   (list
    (read-string "Overleaf URL: " (overleaf--url))))

  (message-box "Log in to overleaf and wait until the browser window closes.")

  (overleaf--with-webdriver
   (unless (and (boundp 'overleaf-cookies)
                (boundp 'overleaf-save-cookies) overleaf-cookies overleaf-save-cookies)
     (user-error "Both overleaf-cookies and overleaf-save-cookies need to be set"))

   (setq-local overleaf-url url)
   (let ((session (make-instance 'webdriver-session)))
     (unwind-protect
         (let ((full-cookies (overleaf--get-full-cookies)))
           (webdriver-session-start session)
           (webdriver-goto-url session (concat (overleaf--url) "/login"))
           (overleaf--message "Log in now...")

           (overleaf--webdriver-wait-until-appears
            (session "//button[@id='new-project-button-sidebar']"))

           (let* ((first-project
                   (webdriver-find-element
                    session
                    (make-instance 'webdriver-by
                                   :strategy "xpath"
                                   :selector "//tr/td/a")))
                  (first-project-path (webdriver-get-element-attribute session first-project "href")))
             (webdriver-goto-url session (concat (overleaf--url) first-project-path))
             (let ((cookies
                    (webdriver-get-all-cookies session)))
               (setf (alist-get (overleaf--cookie-domain) full-cookies nil nil #'string=)
                     (list
                      (substring (apply #'concat
                                        (mapcar (lambda (cookie)
                                                  (format "%s=%s; " (alist-get 'name cookie) (alist-get 'value cookie)))
                                                cookies))
                                 0 -2)
                      (alist-get 'expiry (aref cookies 0))))
               (funcall overleaf-save-cookies
                        (prin1-to-string full-cookies)))))
       (webdriver-session-stop session)))))


;;;###autoload
(defun overleaf-find-file (url)
  "Use selenium webdriver to connect to the project under URL.
To use this, open a file for editing in overleaf in your browser.  Then,
copy the url of the project and use it with this command.

Requires `geckodriver' (see
https://github.com/mozilla/geckodriver/releases) to be installed."
  (interactive
   (list
    (read-string "Overleaf URL: " (overleaf--url))))

  (setq-local overleaf-url url)

  (overleaf--with-webdriver
   (message-box "  1. Wait for the project list to load.
  2. Select a project.
  3. Wait for the project list to load and open a project.
  4. Select the file you would like to edit in the file browser on the left.

This message will self-destruct in 10 seconds!
(Just kidding...)")
   (let ((session (make-instance 'webdriver-session)))
     (unwind-protect
         (progn
           (webdriver-session-start session)
           (webdriver-goto-url session (concat (overleaf--url) "/favicon.svg"))
           (overleaf--webdriver-set-cookies session)
           (webdriver-goto-url session (overleaf--url))

           (overleaf--webdriver-wait-until-appears
            (session "//div[@class='file-tree-inner']")
            (webdriver-execute-synchronous-script session "document.evaluate(\"//div[@class='file-tree-inner']\", document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()" []))

           (overleaf--webdriver-wait-until-appears
            (session "//li[@class='selected']/div" selected)
            (setq-local overleaf-document-id
                        (webdriver-get-element-attribute session selected "data-file-id")))

           (let ((url (webdriver-get-current-url session)))
             (save-match-data
               (let ((match (string-match "^\\(.*\\)/project/\\([0-9a-z]+\\)$" url)))
                 (unless match
                   (user-error "Invalid project url"))
                 (setq-local
                  overleaf-url (match-string 1 url)
                  overleaf-project-id (match-string 2 url))
                 (overleaf-connect)))))
       (webdriver-session-stop session)))))

;;;###autoload
(defun overleaf-connect ()
  "Connect current buffer to overleaf.

Requires `overleaf-cookies' to be set.  Prompts for the
`overleaf-project-id' and `overleaf-document-id' and saves
them in the file."
  (interactive)

  (overleaf-mode t)
  (overleaf-disconnect)
  (if overleaf-cookies
      (let ((overleaf--buffer (current-buffer)))
        (with-current-buffer overleaf--buffer
          (setq-local overleaf-project-id
                      (or overleaf-project-id
                          (read-from-minibuffer "Overleaf project id: ")))
          (setq-local overleaf-document-id
                      (or overleaf-document-id
                          (read-from-minibuffer "Overleaf document id: ")))
          (let* ((cookies (overleaf--get-cookies))
                 (ws-id
                  (car (string-split
                        (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" (overleaf--url) overleaf-project-id)
                          :headers `(("Cookie" . ,cookies)
                                     ("Origin" . ,(overleaf--url)))) ":"))))

            (overleaf--debug "Connecting %s %s" overleaf-project-id overleaf-document-id)

            (setq-local overleaf--last-good-state nil)
            (setq-local overleaf--history '())
            (setq-local overleaf--recent-updates '())
            (setq-local overleaf--last-change-type nil)
            (setq-local overleaf--deletion-buffer "")
            (setq-local overleaf--edit-queue '())
            (setq-local overleaf--send-message-queue '())
            (setq-local overleaf--edit-in-flight nil)
            (setq-local buffer-read-only t)
            (setq-local overleaf--doc-version -1)

            ;; magic value, don't ask me why
            (setq-local overleaf--sequence-id 2)

            (puthash
             (websocket-url
              (setq-local overleaf--websocket
                          (websocket-open
                           (replace-regexp-in-string
                            "https" "wss"
                            (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                                    (overleaf--url) ws-id overleaf-project-id))
                           :on-message #'overleaf--on-message
                           :on-close #'overleaf--on-close
                           :on-open #'overleaf--on-open
                           :custom-header-alist `(("Cookie" . ,cookies)
                                                  ("Origin" . ,(overleaf--url))))))
             overleaf--buffer
             overleaf--ws-url->buffer-table)
            (overleaf--update-modeline))))
    (error "Please set `overleaf-cookies'")))

;;;###autoload
(defun overleaf-disconnect ()
  "Disconnect from overleaf."
  (interactive)
  (when overleaf--websocket
    (overleaf--message "Disconnecting")
    (setq-local overleaf--force-close t)
    (setq-local overleaf--edit-queue '())
    (setq-local overleaf--send-message-queue '())
    (websocket-close overleaf--websocket)
    (when overleaf-auto-save
      (overleaf--save-buffer))
    (setq-local overleaf--force-close nil)
    (remhash overleaf--websocket overleaf--ws-url->buffer-table)))

(defun overleaf--mode-line-item (text &rest help-lines)
  "Return a mode-line item diplaying TEXT having a tooltip with HELP-LINES.
HELP-LINES must consist of string arguments, which are appended
to the default tooltip text."
  `(:propertize
    ,text
    keymap ,overleaf--menu-map
    help-echo ,(string-join (append '("overleaf"
                                      "mouse-1: Display minor mode menu")
                                    help-lines)
                            "\n")))

(defun overleaf--update-modeline ()
  "Update the modeline string to reflect the current connection status."
  (setq-local
   overleaf--mode-line
   (list
    (overleaf--mode-line-item (if overleaf-use-nerdfont "(: " "(Ovl: "))
    (if (websocket-openp overleaf--websocket)
        (list
         (cond ((= overleaf--doc-version -1)
                (overleaf--mode-line-item (if overleaf-use-nerdfont "⟲" "...") "connecting"))
               ((zerop (length overleaf--send-message-queue))
                (overleaf--mode-line-item (if overleaf-use-nerdfont "✓" "C") "connected\nno pending messages"))
               (t
                (overleaf--mode-line-item
                 (format "%i" (length overleaf--send-message-queue))
                 "connected\nnumber of outgoing messages in the queue")))
         (if overleaf-track-changes
             (overleaf--mode-line-item ", t" "track-changes: on")
           ""))
      (overleaf--mode-line-item (if overleaf-use-nerdfont "" "X") "not connected"))
    (overleaf--mode-line-item ")")))
  (force-mode-line-update t))

(defun overleaf--init ()
  "Set up the `overleaf-mode'.

- Add the mode line status to the current mode line string.
- Turn off `inhibit-notification-hooks' as this prevents detecting changes
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
(define-minor-mode overleaf-mode
  "Toggle Overleaf Connection mode.
Interactively with no argument, this command toggles the mode."

  :init-value nil
  :lighter nil ; Use `overleaf--mode-line' instead.
  :keymap
  (list
   (overleaf--key "c" overleaf-connect)
   (overleaf--key "d" overleaf-disconnect)
   (overleaf--key "t" overleaf-toggle-track-changes)
   (overleaf--key "s" overleaf-toggle-auto-save)
   (overleaf--key "b" overleaf-browse-project))

  (if overleaf-mode
      (overleaf--init)
    (setq-local overleaf--mode-line "")
    (force-mode-line-update t)
    (overleaf-disconnect)))


(provide 'overleaf)

;;; overleaf.el ends here
