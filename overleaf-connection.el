;;; overleaf-connection.el --- Sync and track changes live with overleaf. -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2025 Valentin Boettcher


;; Author: Valentin Boettcher
;; Maintainer: Valentin Boettcher <overleaf-connection at protagon.space>
;; Created: March 18, 2025
;; URL: https://github.com/vale981/overleaf-connection.el
;; Package-Requires: ((emacs "30.1") (plz "0.9") (websocket "1.15") (webdriver "0.1"))
;; Version: 1.0.0
;; Keywords: latex, overleaf
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

(require 'overleaf-connection-vars)
(require 'overleaf-connection-utils)
(require 'webdriver)
(require 'webdriver-firefox)
(require 'websocket)
(require 'plz)

;;; Code:

;;;; Communication

(defun overleaf-connection--get-full-cookies ()
  "Load the association list domain<->cookies."
  (if overleaf-connection--current-cookies
      overleaf-connection--current-cookies
    (condition-case err
        (setq overleaf-connection--current-cookies
              (read
               (if (or (functionp overleaf-connection-cookies)
                       (fboundp 'overleaf-connection-cookies))
                   (funcall overleaf-connection-cookies)
                 overleaf-connection-cookies)))
      (error
       (overleaf-connection--warn "Error while loading cookies: %s" (error-message-string err))
       nil))))

(defun overleaf-connection--get-cookies ()
  "Load the cookies from `overleaf-connection-cookies'."
  (if-let
      ((cookies
        (alist-get (overleaf-connection--cookie-domain)
                   (overleaf-connection--get-full-cookies)
                   nil nil #'string=))
       (now (time-convert nil 'integer))) ; Current unix time in seconds.
      (pcase-let ((`(,value ,validity) cookies))
        (if (or (not validity) (< now validity))
            value
          (user-error "Cookies for %s are expired.  Please refresh them using `overleaf-connection-authenticate' or manually"
                      (overleaf-connection--cookie-domain))))
    (user-error "Cookies for %s are not set.  Please set them using `overleaf-connection-get-cookies' or manually"
                (overleaf-connection--cookie-domain))))

(defun overleaf-connection--connected-p ()
  "Return t if the buffer is connected to overleaf."
  (and overleaf-connection--websocket
       (websocket-openp overleaf-connection--websocket)
       (>= overleaf-connection--doc-version 0)))

(defun overleaf-connection--on-open (_websocket)
  "Handle the open even of the web-socket _WEBSOCKET."
  (let ((overleaf-connection--buffer
         (gethash (websocket-url _websocket) overleaf-connection--ws-url->buffer-table)))

    (with-current-buffer overleaf-connection--buffer
      (overleaf-connection--update-modeline)

      (add-hook 'after-change-functions #'overleaf-connection--after-change-function nil :local)
      (add-hook 'before-change-functions #'overleaf-connection--before-change-function nil :local)
      (add-hook 'kill-buffer-hook #'overleaf-connection-disconnect nil :local)

      (setq-local
       overleaf-connection--message-timer
       (progn
         (when overleaf-connection--message-timer
           (cancel-timer overleaf-connection--message-timer))
         (run-at-time t overleaf-connection-message-interval #'overleaf-connection--send-queued-message overleaf-connection--buffer)))

      (setq-local
       overleaf-connection--flush-edit-queue-timer
       (progn
         (when overleaf-connection--flush-edit-queue-timer
           (cancel-timer overleaf-connection--flush-edit-queue-timer))
         (run-with-idle-timer overleaf-connection-flush-interval t #'overleaf-connection--flush-edit-queue overleaf-connection--buffer))))))

(defun overeleaf--on-message (ws frame)
  "Handle a message received from websocket WS with contents FRAME."
  (let ((overleaf-connection--buffer
         (gethash (websocket-url ws) overleaf-connection--ws-url->buffer-table)))
    (overleaf-connection--debug "Got message %S" frame)
    (overleaf-connection--parse-message ws (websocket-frame-text frame))))

(defun overleaf-connection--on-close (ws)
  "Handle the closure of the websocket WS."
  (let ((overleaf-connection--buffer
         (gethash (websocket-url ws) overleaf-connection--ws-url->buffer-table)))
    (with-current-buffer overleaf-connection--buffer
      (when overleaf-connection--websocket
        (overleaf-connection--message "Websocket for document %s closed." overleaf-connection-document-idea)
        (setq-local buffer-read-only nil)
        (cancel-timer overleaf-connection--message-timer)
        (cancel-timer overleaf-connection--flush-edit-queue-timer)
        (remhash (websocket-url ws) overleaf-connection--ws-url->buffer-table)
        (setq-local overleaf-connection--websocket nil)
        (overleaf-connection--update-modeline)
        (unless overleaf-connection--force-close
          (with-current-buffer overleaf-connection--buffer
            (setq buffer-read-only t)
            (sleep-for .1)
            (setq overleaf-connection--websocket nil)
            (overleaf-connect)))))))

(defun overleaf-connection--parse-message (ws message)
  "Parse a message MESSAGE from overleaf, responding by writing to WS."
  (with-current-buffer overleaf-connection--buffer
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
                       (overleaf-connection--is-overleaf-change t))
             (when doc
               (let ((point (point)))
                 (setq-local buffer-read-only nil)
                 (erase-buffer)
                 (insert (overleaf-connection--decode-utf8 (string-join (json-parse-string doc) "\n")))
                 (overleaf-connection--write-buffer-variables)
                 (goto-char point)
                 (setq buffer-undo-list nil))
               (overleaf-connection--set-version version)
               (overleaf-connection--push-to-history version))))
         (overleaf-connection--update-modeline))
        ("5"
         (when message-raw
           (when-let* ((message (json-parse-string message-raw :object-type 'plist :array-type 'list))
                       (name (plist-get message :name)))

             (pcase name
               ("connectionRejected"
                (overleaf-connection--warn "Connection error: %S" (plist-get message :args))
                (overleaf-connection-disconnect))
               ("otUpdateError"
                (overleaf-connection--debug "-------- Update ERROR")
                (overleaf-connection--warn "Update error %S" (car (plist-get message :args)))
                (overleaf-connect))
               ("joinProjectResponse"
                (websocket-send-text ws (format "5:2+::{\"name\":\"joinDoc\",\"args\":[\"%s\",{\"encodeRanges\":true}]}" overleaf-connection-document-idea)))
               ("serverPing"
                (overleaf-connection--debug "Received Ping -> PONG")
                (let ((res (concat id ":::" (json-encode `(:name "clientPong" :args ,(plist-get message :args))))))
                  (websocket-send-text ws res)))
               ("otUpdateApplied"
                (setq overleaf-connection--receiving t)
                (let ((last-version (plist-get (car (plist-get message :args)) :lastV))
                      (version (plist-get (car (plist-get message :args)) :v))
                      (hash (plist-get (car (plist-get message :args)) :hash))
                      (overleaf-connection--is-overleaf-change t)
                      (edits (when (plist-get message :args) (plist-get (car  (plist-get message :args)) :op))))
                  (overleaf-connection--debug "%S Got update with version %s->%s (buffer version %s) %S" (buffer-name) last-version version overleaf-connection--doc-version message)
                  (overleaf-connection--apply-changes edits version last-version hash))
                (setq overleaf-connection--receiving nil)))
             (overleaf-connection--send-queued-message))))))))

(defun overleaf-connection--get-hash ()
  "Get the hash of the overleaf buffer."
  (with-current-buffer overleaf-connection--buffer
    (save-restriction
      (widen)
      (let ((buff (buffer-string)))
        (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff))))))

(defun overleaf-connection--push-to-history (version &optional buffer-string)
  "Push the contents of the buffer or BUFFER-STRING of VERSION to the local overleaf version history."
  (when (and version overleaf-connection--buffer)
    (with-current-buffer overleaf-connection--buffer
      (setq-local overleaf-connection--history
                  (overleaf-connection--splice-into overleaf-connection--history version
                                                    (or buffer-string (buffer-string)) t))
      (setq-local overleaf-connection--history
                  (overleaf-connection--truncate
                   overleaf-connection--history
                   overleaf-connection-history-buffer-length))
      (when overleaf-connection-auto-save
        (save-buffer)))))

(defun overleaf-connection--push-to-recent-updates (update)
  "Splice the UPDATE of type `overleaf-connection--update' into 'overleaf-connection--recent-updates'."
  (let ((from-version (overleaf-connection--update-from-version update))
        (to-version (overleaf-connection--update-to-version update)))
    (when (and from-version to-version overleaf-connection--buffer)
      (with-current-buffer overleaf-connection--buffer
        (setq-local overleaf-connection--recent-updates
                    (overleaf-connection--splice-into
                     overleaf-connection--recent-updates from-version (cons to-version update)))
        (setq-local overleaf-connection--recent-updates
                    (overleaf-connection--truncate
                     overleaf-connection--recent-updates overleaf-connection-update-buffer-length))))))

(defun overleaf-connection--queue-message (message)
  "Queue edit MESSAGE leading to buffer version VERSION to be send to overleaf."
  (setq overleaf-connection--send-message-queue (nconc overleaf-connection--send-message-queue (list message))))

(defun overleaf-connection--send-queued-message (&optional buffer)
  "Send a message from the edit message queue of BUFFER if there is no other edit in flight."
  (let ((overleaf-connection--buffer (or buffer overleaf-connection--buffer)))
    (when overleaf-connection--buffer
      (unless (or overleaf-connection--edit-in-flight overleaf-connection--receiving)
        (with-current-buffer overleaf-connection--buffer
          (when overleaf-connection--send-message-queue
            (when-let* ((message (car overleaf-connection--send-message-queue))
                        (current-version (overleaf-connection--queued-message-doc-version message))
                        (next-version (1+ current-version)))
              (websocket-send-text
               overleaf-connection--websocket
               (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s%s,\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                       (overleaf-connection--queued-message-sequence-id message)
                       overleaf-connection-document-idea
                       overleaf-connection-document-idea
                       (json-encode (apply #'vector (overleaf-connection--queued-message-edits message)))
                       (if (overleaf-connection--queued-message-track-changes message)
                           (format ",\"meta\": {\"tc\":\"%s\"}"
                                   (overleaf-connection--random-string 18))
                         "")
                       next-version
                       current-version
                       (overleaf-connection--queued-message-hash message)))
              (websocket-send-text
               overleaf-connection--websocket
               (format
                "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
                (current-line) (current-column) overleaf-connection-document-idea))

              (setq-local overleaf-connection--edit-in-flight
                          (make-overleaf-connection--update
                           :from-version current-version
                           :to-version next-version
                           :edits (overleaf-connection--queued-message-edits message)
                           :hash (overleaf-connection--queued-message-hash message)
                           :buffer (overleaf-connection--queued-message-buffer message)))
              (overleaf-connection--debug "send %S %i %i" (buffer-name) current-version next-version)
              (setq overleaf-connection--send-message-queue (cdr overleaf-connection--send-message-queue)))))))))

(defun overleaf-connection--apply-changes-internal (edits)
  "Parse the edit list EDITS and apply them to the buffer.

Returns the edits as applied.  This is required because deletions might
no longer be possible, or will occur at a different location."
  (let ((overleaf-connection--is-overleaf-change t))
    (save-excursion
      (remq nil
            (mapcar
             #'(lambda (op)
                 (goto-char (1+ (plist-get op :p)))
                 (if-let* ((insert (plist-get op :i)))
                     (progn
                       (insert insert)
                       (setq buffer-undo-list (memq nil buffer-undo-list))
                       op)
                   (if-let* ((delete (plist-get op :d)))
                       (when (re-search-forward (regexp-quote delete) nil)
                         (replace-match "")
                         (overleaf-connection--debug "applied delete %S %S" op `(:p ,(1- (point)) :d ,delete))
                         (setq buffer-undo-list (memq nil buffer-undo-list))
                         `(:p ,(1- (point)) :d ,delete)))))
             edits)))))

(cl-defun overleaf-connection--apply-changes (edits version last-version hash)
  "Apply change EDITS  from version LAST-VERSION to VERSION to have the hash HASH.

If there are some updates to the buffer that haven't yet been
acknowledged by overleaf or even haven't yet been sent we have to replay
them on top of the changes received from overleaf in the meantime."
  (with-current-buffer overleaf-connection--buffer
    (overleaf-connection--flush-edit-queue overleaf-connection--buffer)
    (let ((point (point)))
      (save-excursion
        (let ((overleaf-connection--is-overleaf-change t))
          (if (and overleaf-connection--edit-in-flight (not last-version))
              (progn
                (overleaf-connection--debug  "%s BINGO, we've been waiting for this. %S %S" (buffer-name) overleaf-connection--doc-version version)
                (setf (overleaf-connection--update-to-version overleaf-connection--edit-in-flight) version)
                (overleaf-connection--push-to-recent-updates
                 overleaf-connection--edit-in-flight)
                (overleaf-connection--push-to-history
                 version
                 (when overleaf-connection--send-message-queue
                   (overleaf-connection--queued-message-buffer-before
                    (car overleaf-connection--send-message-queue))))
                (setq overleaf-connection--edit-in-flight nil))

            (overleaf-connection--set-version version)
            (overleaf-connection--push-to-recent-updates
             (make-overleaf-connection--update :from-version (or last-version (1- version)) :to-version version :edits edits :hash hash)))

          (if (or overleaf-connection--edit-in-flight overleaf-connection--send-message-queue)
              (progn
                (when last-version
                  (if (alist-get last-version overleaf-connection--history)
                      (progn
                        (overleaf-connection--debug "%s replaying" (buffer-name))
                        (erase-buffer)
                        (insert (alist-get last-version overleaf-connection--history))
                        (dolist (change overleaf-connection--recent-updates)
                          (when (>= (car change) last-version)
                            (overleaf-connection--debug "~~~ ----------> %S" change)
                            (overleaf-connection--apply-changes-internal (overleaf-connection--update-edits (cdr (cdr change))))))
                        (overleaf-connection--push-to-history version)


                        (when overleaf-connection--edit-in-flight
                          (overleaf-connection--debug "----------> %S" overleaf-connection--edit-in-flight)
                          (setf (overleaf-connection--update-from-version overleaf-connection--edit-in-flight) overleaf-connection--doc-version)
                          (if (overleaf-connection--apply-changes-internal (overleaf-connection--update-edits overleaf-connection--edit-in-flight))
                              (progn
                                (setf (overleaf-connection--update-buffer overleaf-connection--edit-in-flight) (buffer-string))
                                (overleaf-connection--set-version (1+ overleaf-connection--doc-version)))
                            (overleaf-connection--debug "failed to apply")
                            (setq-local overleaf-connection--edit-in-flight nil))))

                    (overleaf-connection--warn "We haven't seen document version %i yet. Reconnecting" last-version)
                    (overleaf-connect)))

                (when (and overleaf-connection--send-message-queue (or last-version (> version overleaf-connection--doc-version)))
                  (let ((buffer-before-application nil))

                    (when (> version overleaf-connection--doc-version)
                      (erase-buffer)
                      (insert (overleaf-connection--queued-message-buffer-before (car overleaf-connection--send-message-queue))))

                    (setq-local
                     overleaf-connection--send-message-queue
                     (remq nil
                           (mapcar
                            #'(lambda (change)
                                (overleaf-connection--debug "->>>>>>>>>>>>>>>>>>>>>>>>>>>> %S" change)
                                (setq buffer-before-application (buffer-string))
                                (if-let* ((edits
                                           (overleaf-connection--apply-changes-internal
                                            (overleaf-connection--queued-message-edits change))))
                                    (progn
                                      (setf (overleaf-connection--queued-message-doc-version change) overleaf-connection--doc-version)
                                      (setf (overleaf-connection--queued-message-hash change) (overleaf-connection--get-hash))
                                      (setf (overleaf-connection--queued-message-edits change) edits)
                                      (setf (overleaf-connection--queued-message-buffer-before change) buffer-before-application)
                                      (setf (overleaf-connection--queued-message-buffer change) (buffer-string))
                                      (overleaf-connection--set-version (1+ overleaf-connection--doc-version))
                                      change)
                                  (erase-buffer)
                                  (insert buffer-before-application)
                                  nil))
                            overleaf-connection--send-message-queue))))))


            (overleaf-connection--apply-changes-internal edits)
            (overleaf-connection--set-version version)
            (overleaf-connection--push-to-history version))


          (when last-version
            (when (and hash last-version
                       (not overleaf-connection--edit-in-flight)
                       (not overleaf-connection--send-message-queue)
                       (= (- version last-version) 1)
                       (not (string= (overleaf-connection--get-hash) hash)))
              (overleaf-connection--warn "Hash mismatch... reconnecting" (overleaf-connection--get-hash) hash)
              (setq-local buffer-read-only t)
              (overleaf-connect)))))
      (goto-char point))))


;;;; Misc

(defun overleaf-connection--set-version (vers)
  "Set the buffer version to VERS."
  (overleaf-connection--debug "Setting buffer version to %s" vers)
  (setq-local overleaf-connection--doc-version vers))

(defun overleaf-connection--write-buffer-variables ()
  "Write the current buffer-local variables to the buffer."
  (when (overleaf-connection--connected-p)
    (save-excursion
      (let ((overleaf-connection--is-overleaf-change nil)
            (track-changes overleaf-connection-track-changes))
        (setq-local overleaf-connection-track-changes nil)
        (add-file-local-variable 'overleaf-connection-document-idea overleaf-connection-document-idea)
        (add-file-local-variable 'overleaf-connection-project-id overleaf-connection-project-id)
        (add-file-local-variable 'overleaf-connection-track-changes track-changes)
        (add-file-local-variable 'overleaf-connection-auto-save overleaf-connection-auto-save)
        (overleaf-connection--flush-edit-queue (current-buffer))
        (setq-local overleaf-connection-track-changes track-changes)))))

(defun overleaf-connection--save-buffer ()
  "Safely save the buffer."
  (let ((overleaf-connection--is-overleaf-change t))
    (setq-local buffer-read-only t)
    (save-buffer)
    (setq-local buffer-read-only nil)))


;;;; Change Detection

(defvar-local overleaf-connection--before-change "")
(defvar-local overleaf-connection--before-change-begin -1)
(defvar-local overleaf-connection--before-change-end -1)
(defvar-local overleaf-connection--last-change-type nil)
(defvar-local overleaf-connection--last-change-begin 0)
(defvar-local overleaf-connection--last-change-end 0)
(defvar-local overleaf-connection--deletion-buffer "")

(defun overleaf-connection--after-change-function (begin end length)
  "The after change hook that detects a change in region BEGIN - END of length LENGTH to be sent to overleaf."
  (let ((overleaf-connection--buffer (current-buffer)))
    (overleaf-connection--debug "after (%i %i) %i (%i %i) %S" begin end length overleaf-connection--last-change-begin overleaf-connection--last-change-end overleaf-connection--last-change-type)
    (unless overleaf-connection--is-overleaf-change
      (let ((new (buffer-substring-no-properties begin end)))
        ;; the before change hook tends to lie about the end of the region
        (setq overleaf-connection--before-change (substring overleaf-connection--before-change 0 length))
        (setq overleaf-connection--before-change-end (+ overleaf-connection--before-change-begin length))

        (overleaf-connection--debug "change %s -> %s" (json-encode-string overleaf-connection--before-change) (json-encode-string new))
        (unless (and (equal new overleaf-connection--before-change))
          (let ((empty-before (equal overleaf-connection--before-change ""))
                (empty-after (equal new "")))

            (cond
             (empty-before
              (let ((begin-matches (= begin overleaf-connection--last-change-end))
                    (end-matches (= begin overleaf-connection--last-change-begin)))
                (overleaf-connection--debug "insert \"%s\" %S" new overleaf-connection--last-change-begin)

                (if (or (not overleaf-connection--last-change-type) (eq overleaf-connection--last-change-type :d) (not (or begin-matches end-matches)))
                    (progn
                      (overleaf-connection--debug "looks like a new insert!")

                      (setq overleaf-connection--last-change-type :i)
                      (setq overleaf-connection--last-change-end end)
                      (setq overleaf-connection--last-change-begin begin))
                  (setq overleaf-connection--last-change-type :i)

                  ;; extend
                  (cond
                   (begin-matches
                    (overleaf-connection--debug "Extending right to %s" end)
                    (setq overleaf-connection--last-change-end end))
                   (end-matches
                    (overleaf-connection--debug "Extending left to %s" end)
                    (setq overleaf-connection--last-change-begin begin)
                    (setq overleaf-connection--last-change-end (+ overleaf-connection--last-change-end (- end begin))))))))
             (empty-after
              (overleaf-connection--debug "delete")

              (unless overleaf-connection--last-change-type
                (setq-local overleaf-connection--last-change-begin overleaf-connection--before-change-begin)
                (setq-local overleaf-connection--last-change-end end))

              (setq overleaf-connection--last-change-type :d)
              (if (> overleaf-connection--last-change-begin  overleaf-connection--before-change-begin)
                  (progn
                    (setq overleaf-connection--last-change-begin overleaf-connection--before-change-begin)
                    (setq-local overleaf-connection--deletion-buffer (concat overleaf-connection--before-change overleaf-connection--deletion-buffer)))
                (setq-local overleaf-connection--deletion-buffer (concat overleaf-connection--deletion-buffer overleaf-connection--before-change))
                (setq overleaf-connection--last-change-end end))
              (setq-local overleaf-connection--last-change-begin overleaf-connection--before-change-begin))
             (t
              (overleaf-connection--debug "====> Complicated Change")
              (overleaf-connection--debug "====> Restored previous version... replaying")
              (overleaf-connection--debug "====> Deleting %s" (buffer-substring-no-properties begin end))
              (let ((overleaf-connection--is-overleaf-change t))
                (delete-region begin end))
              (setq-local overleaf-connection--last-change-begin overleaf-connection--before-change-begin)
              (setq-local overleaf-connection--last-change-end (+ overleaf-connection--before-change-begin length))
              (setq-local overleaf-connection--last-change-type :d)
              (setq-local overleaf-connection--deletion-buffer overleaf-connection--before-change)
              (overleaf-connection--debug "====> Claiming to have deleted %s" overleaf-connection--before-change)
              (when (< end (point-max))
                (overleaf-connection--debug "====> buffer is now %s" (buffer-substring-no-properties begin end)))

              (overleaf-connection-queue-current-change)
              (overleaf-connection--flush-edit-queue overleaf-connection--buffer)
              (goto-char overleaf-connection--before-change-begin)
              (setq-local overleaf-connection--last-change-begin begin)
              (setq-local overleaf-connection--last-change-end end)
              (setq-local overleaf-connection--last-change-type :i)
              (overleaf-connection--debug "====> Inserting %s" new)
              (let ((overleaf-connection--is-overleaf-change t))
                (insert new))
              (overleaf-connection-queue-current-change)
              (overleaf-connection--flush-edit-queue overleaf-connection--buffer)))))))))

(defun overleaf-connection--before-change-function (begin end)
  "Change hook called to signal an impending change between BEGIN and END.

Mainly used to detect switchover between deletion and insertion."
  (unless overleaf-connection--is-overleaf-change
    (let ((overleaf-connection--buffer (current-buffer)))
      (overleaf-connection--debug "before (%i %i) (%i %i)" begin end overleaf-connection--last-change-begin overleaf-connection--last-change-end)
      (setq-local overleaf-connection--before-change (buffer-substring-no-properties begin end))
      (setq-local overleaf-connection--before-change-begin begin)
      (setq-local overleaf-connection--before-change-end end)
      (when
          (or
           (not (or (= begin overleaf-connection--last-change-begin)
                    (= begin overleaf-connection--last-change-end)
                    (= end overleaf-connection--last-change-begin)
                    (= end overleaf-connection--last-change-end)))
           (and (= end begin) (or (eq overleaf-connection--last-change-type :d)))
           (and (eq overleaf-connection--last-change-type :i) (or (> end begin) (> (length overleaf-connection--before-change) 0)))
           ;; (or (> (max (- end begin) (- overleaf-connection--last-change-end overleaf-connection--last-change-begin))
           ;;        10))
           )
        (overleaf-connection--debug "Edit type switchover --> flushing edit queue %s" overleaf-connection--edit-queue)
        (overleaf-connection-queue-current-change)
        (overleaf-connection--flush-edit-queue (current-buffer))))))

(defun overleaf-connection--flush-edit-queue (buffer)
  "Make an edit message and append it to the message queue of BUFFER."
  (when buffer
    (let ((overleaf-connection--buffer buffer))
      (with-current-buffer buffer
        (overleaf-connection-queue-current-change)
        (when (and overleaf-connection--websocket (websocket-openp overleaf-connection--websocket) overleaf-connection--edit-queue)
          (let ((buf-string (buffer-substring-no-properties (point-min) (point-max))))
            (setq-local buffer-read-only t)
            (overleaf-connection--debug "======> FLUSH %S %i" (buffer-name) overleaf-connection--doc-version)
            (overleaf-connection--queue-message
             (make-overleaf-connection--queued-message
              :sequence-id sequence-id
              :edits (mapcar #'copy-sequence overleaf-connection--edit-queue)
              :doc-version overleaf-connection--doc-version
              :hash (overleaf-connection--get-hash)
              :track-changes overleaf-connection-track-changes
              :buffer-before overleaf-connection--buffer-before-edit-queue
              :buffer buf-string))
            (setq-local overleaf-connection--buffer-before-edit-queue (concat buf-string))
            (setq-local sequence-id (1+ sequence-id))
            (overleaf-connection--set-version (1+ overleaf-connection--doc-version))
            (setq overleaf-connection--edit-queue '())
            (setq-local buffer-read-only nil)))))))

(defun overleaf-connection--queue-edit (edit)
  "Add EDIT to the edit queue."
  (overleaf-connection--debug "====> adding %s to queue" edit)
  (setq overleaf-connection--edit-queue (nconc overleaf-connection--edit-queue (list edit))))


(defun overleaf-connection-queue-current-change (&optional buffer)
  "Queue the change to BUFFER currently being built."
  (let ((overleaf-connection--buffer (or buffer overleaf-connection--buffer)))
    (when overleaf-connection--buffer
      (with-current-buffer overleaf-connection--buffer
        (when (and overleaf-connection--last-change-type (websocket-openp overleaf-connection--websocket))
          (overleaf-connection--debug "======> %s %i %i %s" overleaf-connection--last-change-type overleaf-connection--last-change-begin (point) overleaf-connection--deletion-buffer)
          (pcase overleaf-connection--last-change-type
            (:d
             (overleaf-connection--queue-edit
              `(:p ,(- overleaf-connection--last-change-begin 1) :d ,overleaf-connection--deletion-buffer)))
            (:i
             (overleaf-connection--queue-edit
              `(:p ,(- overleaf-connection--last-change-begin 1) :i ,(buffer-substring-no-properties overleaf-connection--last-change-begin overleaf-connection--last-change-end)))))
          (setq-local overleaf-connection--last-change-type nil)
          (setq-local overleaf-connection--last-change-begin -1)
          (setq-local overleaf-connection--last-change-end -1)
          (setq-local overleaf-connection--deletion-buffer ""))))))

;;;; Interface

;;;###autoload
(defun overleaf-toggle-track-changes ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-connection-track-changes (not overleaf-connection-track-changes))
  (overleaf-connection--write-buffer-variables)
  (overleaf-connection--update-modeline))

;;;###autoload
(defun overleaf-toggle-auto-save ()
  "Toggle track-changes feature change on overleaf."
  (interactive)
  (setq-local overleaf-connection-auto-save (not overleaf-connection-auto-save))
  (overleaf-connection--write-buffer-variables))

;;;###autoload
(defun overleaf-connection-browse-project ()
  "Browse the current project with `browse-url'.
`overleaf-connection-url' and `overleaf-connection-project-id' define the current project."
  (interactive)
  (unless overleaf-connection-project-id
    (user-error "Variable `overleaf-connection-project-id' is not set"))
  (browse-url (format "%s/project/%s" (overleaf-connection--url) overleaf-connection-project-id)))

;;;###autoload
(defun overleaf-connection-authenticate (url)
  "Use selenium webdriver to log into overleaf URL and obtain the cookies.
After running this command, wait for the browser-window to pop up and
for the login page to load.  Note that if the cookies are still valid,
the login page may not be shown and this command terminates without user input.

Requires `geckodriver' (see
https://github.com/mozilla/geckodriver/releases) to be installed."
  (interactive
   (list
    (read-string "Overleaf URL: " (overleaf-connection--url))))

  (message-box "Log in to overleaf and wait until the browser window closes.")

  (overleaf-connection--with-webdriver
   (unless (and (boundp 'overleaf-connection-cookies)
                (boundp 'overleaf-connection-save-cookies) overleaf-connection-cookies overleaf-connection-save-cookies)
     (user-error "Both overleaf-connection-cookies and overleaf-connection-save-cookies need to be set"))

   (setq-local overleaf-connection-url url)
   (let ((session (make-instance 'webdriver-session)))
     (unwind-protect
         (let ((full-cookies (overleaf-connection--get-full-cookies)))
           (webdriver-session-start session)
           (webdriver-goto-url session (concat (overleaf-connection--url) "/login"))
           (overleaf-connection--message "Log in now...")

           (overleaf-connection--webdriver-wait-until-appears
            (session "//button[@id='new-project-button-sidebar']"))

           (let* ((first-project
                   (webdriver-find-element
                    session
                    (make-instance 'webdriver-by
                                   :strategy "xpath"
                                   :selector "//tr/td/a")))
                  (first-project-path (webdriver-get-element-attribute session first-project "href")))
             (webdriver-goto-url session (concat (overleaf-connection--url) first-project-path))
             (let ((cookies
                    (webdriver-get-all-cookies session)))
               (setf (alist-get (overleaf-connection--cookie-domain) full-cookies nil nil #'string=)
                     (list
                      (substring (apply #'concat
                                        (mapcar #'(lambda (cookie)
                                                    (format "%s=%s; " (alist-get 'name cookie) (alist-get 'value cookie)))
                                                cookies))
                                 0 -2)
                      (alist-get 'expiry (aref cookies 0))))
               (funcall overleaf-connection-save-cookies
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
    (read-string "Overleaf URL: " (overleaf-connection--url))))

  (setq-local overleaf-connection-url url)

  (overleaf-connection--with-webdriver
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
           (webdriver-goto-url session (concat (overleaf-connection--url) "/favicon.svg"))
           (overleaf-connection--webdriver-set-cookies session)
           (webdriver-goto-url session (overleaf-connection--url))

           (overleaf-connection--webdriver-wait-until-appears
            (session "//div[@class='file-tree-inner']" file-list)
            (webdriver-execute-synchronous-script session "document.evaluate(\"//div[@class='file-tree-inner']\", document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue.click()" []))

           (overleaf-connection--webdriver-wait-until-appears
            (session "//li[@class='selected']/div" selected)
            (setq-local overleaf-connection-document-idea
                        (webdriver-get-element-attribute session selected "data-file-id")))

           (let ((url (webdriver-get-current-url session)))
             (save-match-data
               (let ((match (string-match "^\\(.*\\)/project/\\([0-9a-z]+\\)$" url)))
                 (unless match
                   (user-error "Invalid project url"))
                 (setq-local
                  overleaf-connection-url (match-string 1 url)
                  overleaf-connection-project-id (match-string 2 url))
                 (overleaf-connect)))))
       (webdriver-session-stop session)))))

;;;###autoload
(defun overleaf-connect ()
  "Connect current buffer to overleaf.

Requires `overleaf-connection-cookies' to be set.  Prompts for the
`overleaf-connection-project-id' and `overleaf-connection-document-idea' and saves
them in the file."
  (interactive)

  (overleaf-connection-mode t)
  (overleaf-connection-disconnect)
  (if overleaf-connection-cookies
      (let ((overleaf-connection--buffer (current-buffer)))
        (with-current-buffer overleaf-connection--buffer
          (setq-local overleaf-connection-project-id
                      (or overleaf-connection-project-id
                          (read-from-minibuffer "Overleaf project id: ")))
          (setq-local overleaf-connection-document-idea
                      (or overleaf-connection-document-idea
                          (read-from-minibuffer "Overleaf document id: ")))
          (let* ((cookies (overleaf-connection--get-cookies))
                 (ws-id
                  (car (string-split
                        (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" (overleaf-connection--url) overleaf-connection-project-id)
                          :headers `(("Cookie" . ,cookies)
                                     ("Origin" . ,(overleaf-connection--url)))) ":"))))

            (overleaf-connection--debug "Connecting %s %s" overleaf-connection-project-id overleaf-connection-document-idea)

            (setq-local overleaf-connection--last-good-state nil)
            (setq-local overleaf-connection--history '())
            (setq-local overleaf-connection--recent-updates '())
            (setq-local overleaf-connection--last-change-type nil)
            (setq-local overleaf-connection--deletion-buffer "")
            (setq-local overleaf-connection--edit-queue '())
            (setq-local overleaf-connection--send-message-queue '())
            (setq-local overleaf-connection--edit-in-flight nil)
            (setq-local buffer-read-only t)
            (setq-local overleaf-connection--doc-version -1)
            (setq-local sequence-id 2)
            (puthash
             (websocket-url
              (setq-local overleaf-connection--websocket
                          (websocket-open
                           (replace-regexp-in-string
                            "https" "wss"
                            (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                                    (overleaf-connection--url) ws-id overleaf-connection-project-id))
                           :on-message #'overeleaf--on-message
                           :on-close #'overleaf-connection--on-close
                           :on-open #'overleaf-connection--on-open
                           :custom-header-alist `(("Cookie" . ,cookies)
                                                  ("Origin" . ,(overleaf-connection--url))))))
             overleaf-connection--buffer
             overleaf-connection--ws-url->buffer-table)
            (overleaf-connection--update-modeline))))
    (error "Please set `overleaf-connection-cookies'")))

;;;###autoload
(defun overleaf-connection-disconnect ()
  "Disconnect from overleaf."
  (interactive)
  (when overleaf-connection--websocket
    (overleaf-connection--message "Disconnecting")
    (setq-local overleaf-connection--force-close t)
    (setq-local overleaf-connection--edit-queue '())
    (setq-local overleaf-connection--send-message-queue '())
    (websocket-close overleaf-connection--websocket)
    (when overleaf-connection-auto-save
      (overleaf-connection--save-buffer))
    (setq-local overleaf-connection--force-close nil)
    (remhash overleaf-connection--websocket overleaf-connection--ws-url->buffer-table)))

(defun overleaf-connection--update-modeline ()
  "Update the modeline string to reflect the current connection status."
  (setq-local overleaf-connection--mode-line
              (concat
               "(: "
               (if (websocket-openp overleaf-connection--websocket)
                   (concat "["
                           (if (= overleaf-connection--doc-version -1)
                               "⟲"
                             "✓")
                           (format ", %i" (length overleaf-connection--send-message-queue))
                           (if overleaf-connection-track-changes
                               ", t"
                             "")
                           "]")
                 "[ ]")
               ")"))
  (force-mode-line-update t))

(defun overleaf-connection--init ()
  "Set up the `overleaf-connection-mode'.

- Add the mode line status to the current mode line string.
- Turn off `inhibit-notification-hooks' as this prevents detecting changes
  to sync to overleaf."

  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'overleaf-connection--mode-line global-mode-string)
    (setq global-mode-string (append global-mode-string
                                     '(overleaf-connection--mode-line))))

  (overleaf-connection--update-modeline)
  (setq inhibit-modification-hooks nil))


(defmacro overleaf-connection--key (key function)
  "Define a mapping of KEY to FUNCTION with the appropriate prefix."
  `(cons (kbd ,(concat overleaf-connection-keymap-prefix " " key))  #',function))



;;;###autoload
(define-minor-mode overleaf-connection-mode
  "Toggle Overleaf Connection mode.
Interactively with no argument, this command toggles the mode."

  :init-value nil
  :ligther " Overleaf"
  :keymap
  (list
   (overleaf-connection--key "c" overleaf-connect)
   (overleaf-connection--key "d" overleaf-connection-disconnect)
   (overleaf-connection--key "t" overleaf-toggle-track-changes)
   (overleaf-connection--key "s" overleaf-toggle-auto-save)
   (overleaf-connection--key "b" overleaf-connection-browse-project))

  (if overleaf-connection-mode
      (overleaf-connection--init)
    (setq-local overleaf-connection--mode-line "")
    (force-mode-line-update t)
    (overleaf-connection-disconnect)))


(provide 'overleaf-connection)

;;; overleaf-connection.el ends here
