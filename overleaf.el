;;; overleaf.el --- Sync and track changes live with overleaf -*- lexical-binding: t; -*-
;; Copyright (C) 2020-2025 Valentin Boettcher


;; Author: Valentin Boettcher
;; Maintainer: Valentin Boettcher <overleaf at protagon.space>
;; Created: March 18, 2025
;; URL: https://github.com/vale981/overleaf.el
;; Package-Requires: ((emacs "29.4") (plz "0.9") (websocket "1.15") (webdriver "0.1") (posframe "1.4.4"))
;; Version: 1.1.3
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
(require 'posframe)
(require 'xref)

;;; Code:

;;;; Variables

(defgroup overleaf nil
  "Sync and track changes live with overleaf."
  :prefix "overleaf-"
  :group 'tools)

(defcustom overleaf-user-colors
  ["green" "red" "blue" "pink" "goldenrod"]
  "The colors used to display cursors and names."
  :type '(repeat color)
  ;; Customize UI cannot work with variable length vectors, so we need
  ;; these conversion functions.
  :get (lambda (name) (append (symbol-value name) nil))
  :set (lambda (name value) (set name (vconcat value))))

(defcustom overleaf-cursor-gravatars nil
  "Display gravatars at other users' cursor positions."
  :type 'boolean)

(defvar overleaf-cookies nil
  "The overleaf session cookies.

Can a string or function that returns a string containing the overleaf
authentication cookies.

For example the variable can be bound to a function that loads the
cookies from a gpg encrypted file.  See
`overleaf-read-cookies-from-file' or `overleaf-read-cookies-from-firefox'..

The cookies are most easily obtained from the developer tools in the
browser.")

(defun overleaf--buffer-string (&optional start end)
  "Return the buffer contents between START and END with no text properties.
If START and END are nil, return the whole buffer.
Widen the buffer before extracting the contents."
  (save-restriction
    (widen)
    (buffer-substring-no-properties
     (or start (point-min))
     (or end (point-max)))))

;;;###autoload
(defun overleaf-read-cookies-from-file (file)
  "Return a cookie saving function to load the cookie-string from FILE.
To be used with `overleaf-cookies'."
  (lambda ()
    (with-temp-buffer
      (insert-file-contents (expand-file-name file))
      (read (string-trim (buffer-string))))))

;;;###autoload
(cl-defun overleaf-read-cookies-from-firefox (&key (firefox-folder "~/.mozilla/firefox/") (profile nil))
  "Make a cookie saving function reading the database at FIREFOX-FOLDER.

To be used with `overleaf-cookies'.  The Firefox folder should be
located at `~/.mozilla/firefox/'.  If PROFILE is provided, choose this
profile.  Otherwise prompt."
  (lambda ()
    (setopt overleaf-cache-cookies nil)
    (if (sqlite-available-p)
        (let* ((profile-blocks
                (with-temp-buffer
                  (insert-file-contents (expand-file-name (concat firefox-folder "/profiles.ini")))
                  (goto-char (point-min))
                  (let ((matches))
                    (while (re-search-forward "\\[.*\\]\\(\\(?:.\\|\n\\)+?\\)\\[" (point-max) t)
                      (backward-char)
                      (push (match-string 1) matches))
                    matches)))

               (profiles
                (remq 'nil
                      (mapcar (lambda (block)
                                (save-match-data
                                  (when-let*
                                      ((path (progn (string-match "^Path=\\(.*?\\)$" block)
                                                    (match-string 1 block)))
                                       (name (progn (string-match "^Name=\\(.*?\\)$" block)
                                                    (match-string 1 block))))
                                    `(:fields (,name) :data ,path))))
                              profile-blocks)))

               (profile (if profile
                            (plist-get
                             (seq-find
                              (lambda (row)
                                (string= (car (plist-get row :fields)) profile))
                              profiles)
                             :data)
                          (overleaf--completing-read "Profile: " profiles)))

               (cookie-file
                (if profile
                    (expand-file-name (concat firefox-folder "/" profile "/cookies.sqlite"))
                  (user-error "Profile does not exist")))
               (db (sqlite-open cookie-file))
               (result
                (mapcar
                 (lambda (row)
                   (pcase-let* ((`(,domain ,name ,value ,expiry) row))
                     `(,domain
                       ,(format "%s=%s" name value)
                       ,expiry)))
                 (sqlite-select
                  db "SELECT host, name, value, expiry FROM moz_cookies WHERE name = 'overleaf_session2' OR name = 'overleaf.sid'"))))

          (sqlite-close db)
          result)
      (user-error "Sqlite not available!"))))

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

(defcustom overleaf-default-url "https://www.overleaf.com"
  "The default url of the overleaf server."
  :type 'string)

(defcustom overleaf-flush-interval .5
  "The idle-timer delay to flush the edit queue."
  :type 'float)

(defcustom overleaf-debug nil
  "Whether to log debug messages."
  :type 'boolean)

(defcustom overleaf-use-nerdfont nil
  "Whether to use nerd-font icons for the modeline."
  :type 'boolean)

(defcustom overleaf-context-size 10
  "Standard context window size for finding where to re-apply an edit."
  :type 'integer)

(defcustom overleaf-cache-cookies t
  "Whether to cache the cookies after obtaining them.

This does nothing if `overleaf-read-cookies-from-firefox'
or `overleaf-read-cookies-from-chromium' is used."
  :type 'boolean)

(defcustom overleaf-user-info-template "%n\t%e\t%i"
  "The `format-spec' template used in `overleaf-list-users'.
The following specification characters can be used:
%n -- user's name
%e -- user's email address
%i -- user's internal ID
%w -- `which-function' at user's current cursor position
      (produces an empty string if `which-function-mode' is not enabled)"
  :type 'string
  :group 'overleaf-mode)

(defcustom overleaf-webdriver-init-function #'overleaf--webdriver-make-session
  "Function called to create a webdriver session."
  :type 'function
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

(defvar-local overleaf-url nil
  "The url of the overleaf server.")


;;;###autoload
(eval-and-compile
  (put 'overleaf-auto-save 'safe-local-variable #'booleanp)
  (put 'overleaf-url 'safe-local-variable #'stringp)
  (put 'overleaf-track-changes 'safe-local-variable #'booleanp)
  (put 'overleaf-project-id 'safe-local-variable #'overleaf-id-p)
  (put 'overleaf-document-id 'safe-local-variable #'overleaf-id-p)
  ;; Do not ignore :propertize forms.  See variable `mode-line-format'.
  (put 'overleaf--mode-line 'risky-local-variable t))

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

(defvar-local overleaf--edit-queue '()
  "A list of edits that can be send in one go.")

(defvar-local overleaf--edits-in-flight nil
  "If non-nil, we still await the acknowledgment from overleaf.")

(defvar-local overleaf--doc-version -2
  "Current version of the document.")

(defvar-local overleaf--sequence-id -1
  "An internal counter for the websocket communication.")

(defvar-local overleaf--mode-line ""
  "Contents of the mode-line indicator.")

(defvar overleaf--ws-url->buffer-table (make-hash-table :test #'equal)
  "A hash table associating web-sockets to buffers.")

(defvar overleaf--buffer nil
  "The current overleaf buffer (used in lexical binding).")

(defvar-local overleaf--user-id ""
  "The public id of the user.")

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

(easy-menu-define overleaf-menu nil
  "Overleaf menu."
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
     :active overleaf-project-id]
    ["Goto cursor" overleaf-goto-cursor
     :help "Jump to the cursor of another user."
     :active (overleaf--other-users-p)]
    ["List active users" overleaf-list-users
     :help "List other active users in an xref buffer"
     :active (overleaf--other-users-p)]))

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
  buffer
  context)

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

(defmacro overleaf--save-context (&rest body)
  "Like `save-excursion' but using text search with context window.

Executes BODY and then tries to reset the point.
The context window size is configured using `overleaf-context-size'."
  (let ((pos (gensym))
        (context-before (gensym))
        (context-after (gensym))
        (context (gensym)))
    `(let ((,pos (point)))
       (cl-destructuring-bind (,context-before ,context-after ,context) (overleaf--extract-context-at-point)
         ,@body
         (save-restriction
           (widen)
           (goto-char ,pos)
           (when (> ,pos 1)
             (ignore-errors (backward-char (1+ (length ,context-before)))))
           (if (search-forward ,context nil t)
               (ignore-errors (backward-char (length ,context-after)))
             (if (search-backward ,context nil t)
                 (ignore-errors (forward-char (length ,context-before)))
               (goto-char ,pos))))))))

;;;; Logging
(defsubst overleaf--debug (format-string &rest args)
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

;;;; Communication

(defun overleaf--get-full-cookies ()
  "Load the association list domain<->cookies."
  (if (and overleaf--current-cookies overleaf-cache-cookies)
      overleaf--current-cookies
    (condition-case err
        (setq overleaf--current-cookies
              (if (or (functionp overleaf-cookies)
                      (fboundp 'overleaf-cookies))
                  (funcall overleaf-cookies)
                overleaf-cookies))
      (error
       (overleaf--warn "Error while loading cookies: %s" (error-message-string err))
       nil))))

(defun overleaf--get-cookies ()
  "Load the cookies from `overleaf-cookies'."
  (if-let*
      ((cookies
        (cl-some (lambda (prefix)
                   (alist-get (concat prefix (overleaf--cookie-domain))
                              (overleaf--get-full-cookies)
                              nil nil #'string=))
                 '("." "")))
       (now (time-convert nil 'integer))) ; Current unix time in seconds.
      (pcase-let ((`(,value ,validity) cookies))
        (if (or (not validity) (< now validity))
            value
          (setq overleaf--current-cookies nil)
          (user-error "Cookies for %s are expired.  Please refresh them using `overleaf-authenticate' or manually"
                      (overleaf--cookie-domain))))
    (setq overleaf--current-cookies nil)
    (user-error "Cookies for %s are not set.  Please set them using `overleaf-authenticate' or manually"
                (overleaf--cookie-domain))))

(defun overleaf--connected-p ()
  "Return t if the buffer is connected to overleaf."
  (and overleaf--websocket
       (websocket-openp overleaf--websocket)
       (>= overleaf--doc-version -1)))

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
       overleaf--flush-edit-queue-timer
       (progn
         (when overleaf--flush-edit-queue-timer
           (cancel-timer overleaf--flush-edit-queue-timer))
         (run-with-idle-timer overleaf-flush-interval t
                              (lambda (buffer)
                                (with-current-buffer buffer
                                  (unless overleaf--receiving
                                    (overleaf--flush-edit-queue buffer))))
                              overleaf--buffer))))))

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
    (when overleaf--buffer
      (with-current-buffer overleaf--buffer
        (when overleaf--websocket
          (overleaf--message "Websocket for document %s closed." overleaf-document-id)
          (setq-local buffer-read-only nil)
          (cancel-timer overleaf--flush-edit-queue-timer)
          (remhash (websocket-url ws) overleaf--ws-url->buffer-table)
          (setq-local overleaf--websocket nil)
          (overleaf--update-modeline)
          (unless overleaf--force-close
            (with-current-buffer overleaf--buffer
              (setq buffer-read-only t)
              (sleep-for .1)
              (setq overleaf--websocket nil)
              (overleaf-connect))))))))

(defun overleaf--parse-message (ws message)
  "Parse a message MESSAGE from overleaf, responding by writing to WS."
  (with-current-buffer overleaf--buffer
    (setq-local overleaf--receiving t)

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
               (let ((hash (and (not (buffer-modified-p))
                                (save-restriction (widen) (buffer-hash)))))
                 (setq-local buffer-read-only nil)
                 (overleaf--reset-buffer-to
                  (overleaf--decode-utf8 (string-join (json-parse-string doc) "\n")))

                 (setq buffer-undo-list nil)
                 (overleaf--set-version version)
                 (overleaf--push-to-history version)

                 ;; Copied from `fill-paragraph':
                 ;; If we didn't change anything in the buffer (and the buffer
                 ;; was previously unmodified), then flip the modification status
                 ;; back to "unchanged".
                 (when (and hash
                            (equal hash (save-restriction (widen) (buffer-hash))))
                   (set-buffer-modified-p nil))))
             (run-with-idle-timer
              1 nil
              (lambda (buffer)
                (with-current-buffer buffer
                  (overleaf--write-buffer-variables)))
              overleaf--buffer)
             (overleaf--update-modeline))))
        ("5"
         (when message-raw
           (when-let* ((message (json-parse-string message-raw :object-type 'plist :array-type 'list))
                       (name (plist-get message :name)))
             (pcase name
               ("clientTracking.clientUpdated"
                (let ((args (car (plist-get message :args))))
                  (overleaf--update-cursor
                   (plist-get args :id)
                   (plist-get args :name)
                   (plist-get args :email)
                   (plist-get args :row)
                   (plist-get args :column))))
               ("clientTracking.clientDisconnected"
                (let ((id (car (plist-get message :args))))
                  (overleaf--remove-cursor id)))
               ("connectionRejected"
                (overleaf--warn "Connection error: %S" (plist-get message :args))
                (overleaf-disconnect))
               ("otUpdateError"
                (overleaf--debug "-------- Update ERROR")
                (overleaf--warn "Update error %S" (car (plist-get message :args)))
                (overleaf-connect))
               ("joinProjectResponse"
                (setq-local overleaf--user-id
                            (plist-get
                             (car (plist-get message :args))
                             :publicId))
                (unwind-protect
                    (unless overleaf-document-id
                      (let* ((root
                              (overleaf--pget
                               (car (plist-get message :args))
                               :project
                               :rootFolder 0))
                             (collection
                              (overleaf--get-files root)))
                        (setq-local overleaf-document-id
                                    (overleaf--completing-read
                                     "Select file: "
                                     collection))))

                  (if overleaf-document-id
                      (websocket-send-text ws (format "5:2+::{\"name\":\"joinDoc\",\"args\":[\"%s\",{\"encodeRanges\":true}]}" overleaf-document-id))
                    (overleaf--warn "Invalid document id %S" overleaf-document-id)
                    (overleaf-disconnect))))
               ("serverPing"
                (overleaf--debug "Received Ping -> PONG")
                (let ((res (concat id ":::" (json-encode `(:name "clientPong" :args ,(plist-get message :args))))))
                  (websocket-send-text ws res)))
               ("otUpdateApplied"
                (let ((last-version (plist-get (car (plist-get message :args)) :lastV))
                      (version (plist-get (car (plist-get message :args)) :v))
                      (hash (plist-get (car (plist-get message :args)) :hash))
                      (overleaf--is-overleaf-change t)
                      (edits (when (plist-get message :args) (plist-get (car  (plist-get message :args)) :op))))
                  (overleaf--debug "%S Got update with version %s->%s (buffer version %s) %S" (buffer-name) last-version version overleaf--doc-version message)
                  (overleaf--apply-changes edits version last-version hash))
                (overleaf--send-position-update))))))))
    (setq-local overleaf--receiving nil)))

(defun overleaf--get-files (folder &optional parent)
  "Recursively parse overleafs FOLDER structure to list all documents.

Optionally a PARENT prefix string may be provided."
  (let* ((docs
          (plist-get folder :docs))
         (folders
          (plist-get folder :folders))
         (name (let ((tmpname (plist-get folder :name)))
                 (if (string= tmpname "rootFolder")
                     ""
                   tmpname)))
         (parent (concat (or parent "") name "/")))
    (apply #'nconc
           (mapcar
            (lambda (file)
              `(:fields (,(concat parent  (plist-get file :name))) :data ,(plist-get file :_id)))
            docs)
           (mapcar
            (lambda (folder)
              (overleaf--get-files folder parent))
            folders))))

(defun overleaf--get-hash ()
  "Get the hash of the overleaf buffer."
  (with-current-buffer overleaf--buffer
    (let ((buff (overleaf--buffer-string)))
      (secure-hash 'sha1 (format "blob %i\x00%s" (length buff) buff)))))

(defun overleaf--push-to-history (version &optional buffer-string)
  "Push the buffer to the version history.
Push the contents of the buffer or BUFFER-STRING of VERSION to the local
overleaf version history."
  (when (and version overleaf--buffer)
    (with-current-buffer overleaf--buffer
      (setq-local overleaf--history
                  (overleaf--splice-into overleaf--history version
                                         (or buffer-string (overleaf--buffer-string)) t))
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
                     overleaf--recent-updates from-version (cons to-version update)
                     t))
        (setq-local overleaf--recent-updates
                    (overleaf--truncate
                     overleaf--recent-updates overleaf-update-buffer-length))))))

(defun overleaf--send-position-update ()
  "Send a position tracking update to overleaf."
  (with-current-buffer overleaf--buffer
    (when (overleaf--connected-p)
      (save-restriction
        (widen)
        (websocket-send-text
         overleaf--websocket
         (format
          "5:::{\"name\":\"clientTracking.updatePosition\",\"args\":[{\"row\":%i,\"column\":%i,\"doc_id\":\"%s\"}]}"
          (1- (line-number-at-pos)) (- (point) (line-beginning-position)) overleaf-document-id))))))

(defun overleaf--apply-changes-internal (edits)
  "Parse the edit list EDITS and apply them to the buffer.

Returns the edits as applied.  This is required because deletions might
no longer be possible, or will occur at a different location.  If CONTEXT
is provided use the context to fine tune where the edit is applied."
  (let ((overleaf--is-overleaf-change t)
        (new-edits nil))
    (save-restriction
      (widen)
      (dolist (op edits)
        (let ((pos (1+ (plist-get op :p))))
          (goto-char pos)
          (if-let* ((insert (plist-get op :i)))
              (progn
                (insert insert)
                (setq buffer-undo-list (memq nil buffer-undo-list))
                (push `(:p ,(1- pos) :i ,insert) new-edits))

            (if-let* ((delete (plist-get op :d)))
                (save-match-data
                  (if (re-search-forward (regexp-quote delete) nil t)
                      (progn
                        (let ((delete-loc (1- (- (point) (length delete)))))
                          (replace-match "")
                          (setq buffer-undo-list (memq nil buffer-undo-list))
                          (push `(:p ,delete-loc :d ,delete) new-edits)))
                    (setq edits nil))))))))
    (nreverse new-edits)))

(defun overleaf--verify-buffer (hash)
  "Verify that the current buffer has the hash HASH.
Re-connect if this is not the case."
  (overleaf--debug "Verify")
  (when (and hash
             (not (string= (overleaf--get-hash) hash)))
    (overleaf--debug "%s" (overleaf--buffer-string))
    (overleaf--warn "Hash mismatch... reconnecting")
    (setq-local buffer-read-only t)
    (overleaf-connect)))

(defun overleaf--transform-edits (edits history-edits)
  "Transform EDITS to new positions by HISTORY-EDITS that came after the EDITS."
  (dolist (history-op history-edits)
    (setq edits
          (mapcar
           (lambda (op)
             (let ((history-position (plist-get history-op :p))
                   (position (plist-get op :p)))
               (if (<= history-position position)
                   (progn
                     (overleaf--debug "--------> updating: history: %S  this: %S" history-op op)

                     (plist-put
                      op :p
                      (+ position (let* ((insert (plist-get history-op :i))
                                         (delete (plist-get history-op :d)))
                                    (+ (if insert (length insert) 0)
                                       (if delete (* -1 (length delete)) 0))))))
                 op)))
           edits)))
  edits)

(defun overleaf--apply-changes (edits version last-version hash)
  "Apply change EDITS  from version LAST-VERSION to VERSION to have the hash HASH.

If there are some updates to the buffer that haven't yet been
acknowledged by overleaf or even haven't yet been sent we have to replay
them on top of the changes received from overleaf in the meantime."
  (overleaf--debug "VERSION: %S -> %S Current: %S Edits: %S In flight: %S" last-version version overleaf--doc-version edits overleaf--edits-in-flight)
  (overleaf-queue-pending-edit)
  (let ((overleaf--is-overleaf-change t))
    (if (and overleaf--edits-in-flight (not edits))
        (progn
          (let ((update (car overleaf--edits-in-flight)))
            (overleaf--debug "%S BINGO, we've been waiting for this. %S %S" (buffer-name) (overleaf--update-to-version update) version)
            (setf (overleaf--update-to-version update) version)
            (overleaf--push-to-recent-updates
             update)
            (overleaf--push-to-history version (overleaf--buffer-string))
            (setf (overleaf--update-to-version update) version)
            (setf (overleaf--update-from-version update) (1- version))
            (overleaf--push-to-recent-updates update)
            (overleaf--set-version version))
          (setq overleaf--edits-in-flight (cdr overleaf--edits-in-flight)))

      (when edits
        (overleaf--save-context
         (with-current-buffer overleaf--buffer
           (when overleaf--edit-queue
             (overleaf--debug "-------------> RESET")
             (overleaf--reset-buffer-to overleaf--buffer-before-edit-queue))

           (when overleaf--edits-in-flight
             (overleaf--reset-buffer-to (overleaf--update-buffer (car overleaf--edits-in-flight))))

           (overleaf--apply-changes-internal edits)
           (overleaf--set-version version)
           (when (and hash (not overleaf--edits-in-flight)) (overleaf--verify-buffer hash))
           (overleaf--push-to-history version)
           (overleaf--push-to-recent-updates
            (make-overleaf--update
             :edits edits
             :from-version (1- version) ;; we updated it so that there's no jump
             :to-version version
             :edits edits
             :buffer ""))

           (if overleaf--edit-queue
               (progn
                 (when edits
                   (setq overleaf--edit-queue (overleaf--transform-edits overleaf--edit-queue edits)))
                 (setq overleaf--buffer-before-edit-queue (overleaf--buffer-string))
                 (setq overleaf--edit-queue (overleaf--apply-changes-internal overleaf--edit-queue)))
             (overleaf--reset-edit-queue))))))

    (overleaf--update-modeline)))

;;;; Misc

;;;###autoload
(defun overleaf-id-p (id)
  "Return t if ID is an overleaf project/document id."
  (string-match-p "^[a-z0-9]+$" (format "%s" id)))

(defun overleaf--pget (plist &rest keys)
  "Recursively find KEYS in PLIST."
  (while keys
    (let ((key (pop keys)))
      (if (integerp key)
          (setq plist (nth key plist))
        (setq plist (plist-get plist key)))))
  plist)

(defun overleaf--completing-read (prompt collection &optional padding)
  "Perform a completing read with PROMPT.

The COLLECTION contains row of the form `(:fields ([string 1] \...)
:data [data] [optional: :propertize [arguments to propeRTIZE-string]] )'
where the fields are displayed as columns of the table separated by
PADDING (2 characters by default)."
  (let* ((num-fields (1- (length (plist-get (car collection) :fields))))
         (padding (or padding 2))
         (field-widths
          (mapcar
           (lambda (field)
             (apply #'max
                    (mapcar
                     (lambda (row)
                       (length (nth field (plist-get row :fields))))
                     collection)))
           (number-sequence 0 num-fields)))
         (format-string
          (apply #'concat
                 (mapcar (lambda (width)
                           (format "%%-%is" (+ padding width)))
                         field-widths)))
         (final-collection
          (cl-loop for row in collection
                   collect `(,(let ((str (apply #'format format-string (plist-get row :fields))))
                                (if-let* ((args (plist-get row :propertize)))
                                    (apply #'propertize str args)
                                  str))
                             ,(plist-get row :data)))))
    (car (cdr
          (assoc
           (completing-read prompt
                            final-collection
                            nil t)
           final-collection)))))

(defun overleaf--set-version (vers)
  "Set the buffer version to VERS."
  (overleaf--debug "Setting buffer version to %s" vers)
  (setq-local overleaf--doc-version vers))

(defun overleaf--write-buffer-variables ()
  "Write the current buffer-local variables to the buffer."
  (when (overleaf--connected-p)
    (let ((overleaf--is-overleaf-change nil)
          (track-changes overleaf-track-changes))
      (save-excursion
        (save-restriction
          (setq-local overleaf-track-changes nil)
          (add-file-local-variable 'overleaf-document-id overleaf-document-id)
          (add-file-local-variable 'overleaf-project-id overleaf-project-id)
          (add-file-local-variable 'overleaf-track-changes track-changes)
          (add-file-local-variable 'overleaf-auto-save overleaf-auto-save)
          (add-file-local-variable 'overleaf-url overleaf-url)))
      (overleaf--flush-edit-queue (current-buffer))
      (setq-local overleaf-track-changes track-changes))))

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
        `(,(car list) ,@(overleaf--splice-into (cdr list) vers content unique))))))

(defun overleaf--truncate (list max-length)
  "Remove the first elements from LIST to make it MAX-LENGTH long."
  (nthcdr (max 0 (- (length list) max-length)) list))

(defun overleaf--url ()
  "Return a sanitized version of the url without trailing slash."
  (string-trim (string-trim (or overleaf-url overleaf-default-url)) "" "/"))

(defun overleaf--cookie-domain ()
  "Return the domain for which the cookies will be valid.
The value is computed from the current value of `overleaf-url'."
  (let ((domain-parts (string-split (replace-regexp-in-string ".*?://" "" (overleaf--url)) "\\.")))
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

(defun overleaf--extract-context-at-point (&optional length)
  "Return context `(before after total)' of length LENGTH around the point."
  (let* ((length (or length overleaf-context-size))
         (pos (point))
         (context-before (overleaf--buffer-string (max 1 (- pos length)) pos))
         (context-after (overleaf--buffer-string pos (min (point-max) (+ pos length))))
         (context (concat context-before context-after)))
    (overleaf--debug "using context: %S %S %S" context-before context-after context)
    (list context-before context-after context)))

(defun overleaf--reset-buffer-to (contents)
  "Erase the buffer and insert CONTENTS while trying to keep the point position."
  (overleaf--save-context
   (save-restriction
     (widen)
     (erase-buffer)
     (insert contents))))

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

(defun overleaf--webdriver-make-session ()
  (make-instance 'webdriver-session))

;;;; Change Detection

(defvar-local overleaf--before-change "")
(defvar-local overleaf--before-change-begin -1)
(defvar-local overleaf--before-change-end -1)
(defvar-local overleaf--last-change-type nil)
(defvar-local overleaf--last-change-begin 0)
(defvar-local overleaf--last-change-end 0)
(defvar-local overleaf--deletion-buffer "")
(defvar-local overleaf--change-context nil)
(defvar-local overleaf--buffer-before-change nil)

(defun overleaf--after-change-function (begin end length)
  "The after change hook that is called after the buffer changed.
The change is in the region BEGIN - END of length LENGTH to be sent to
overleaf."
  (let ((overleaf--buffer (current-buffer)))
    (overleaf--debug "after (%i %i) %i (%i %i) %S" begin end length overleaf--last-change-begin overleaf--last-change-end overleaf--last-change-type)
    (unless overleaf--is-overleaf-change
      (if overleaf--receiving
          (overleaf--reset-buffer-to overleaf--buffer-before-change)
        (let ((new (overleaf--buffer-string begin end)))

          (unless overleaf--change-context
            (save-excursion
              (let ((buf-text overleaf--buffer-before-edit-queue)
                    (pos overleaf--before-change-begin))
                (setq-local overleaf--change-context
                            (with-temp-buffer
                              (insert buf-text)
                              (goto-char pos)
                              (overleaf--extract-context-at-point))))))


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
                (overleaf-queue-pending-edit)
                (let* ((old overleaf--before-change)
                       (final (let* ((kept-length (- (length old) length))
                                     (new-length (- end begin))
                                     (final-length (+ kept-length new-length))
                                     (final-begin overleaf--before-change-begin)
                                     (final-end (+ final-begin final-length)))
                                (overleaf--buffer-string final-begin final-end))))
                  (overleaf--debug "====> Complicated Change (Replacement): Deleting '%s', Inserting '%s'"
                                   old final)
                  (overleaf--queue-edit
                   `(:p ,(- overleaf--before-change-begin 1) :d ,old))
                  (overleaf--queue-edit
                   `(:p ,(- overleaf--before-change-begin 1) :i ,final))))))))))))

(defun overleaf--before-change-function (begin end)
  "Change hook called to signal an impending change between BEGIN and END.

Mainly used to detect switchover between deletion and insertion."
  (unless overleaf--is-overleaf-change
    (let ((overleaf--buffer (current-buffer)))
      (if overleaf--receiving
          (setq overleaf--buffer-before-change (overleaf--buffer-string))
        (overleaf--debug "before (%i %i) (%i %i)" begin end overleaf--last-change-begin overleaf--last-change-end)
        (setq-local overleaf--before-change (overleaf--buffer-string begin end))
        (setq-local overleaf--before-change-begin begin)
        (setq-local overleaf--before-change-end end)
        (unless overleaf--change-context
          (save-excursion
            (goto-char begin)
            (setq-local overleaf--change-context (overleaf--extract-context-at-point))
            (overleaf--debug "Change context: %S" overleaf--change-context)))
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
          (overleaf-queue-pending-edit))))))

(defun overleaf--reset-edit-queue ()
  "Empty the edit queue and reset buffer to before the edit queue."
  (setq-local overleaf--buffer-before-edit-queue (overleaf--buffer-string))
  (setq overleaf--edit-queue '())
  (setq overleaf--change-context nil))

(defun overleaf--flush-edit-queue (buffer)
  "Send an edit message and append it to the edit queue of BUFFER."
  (when buffer
    (let ((overleaf--buffer buffer))
      (with-current-buffer buffer
        (overleaf-queue-pending-edit)
        (overleaf--send-position-update)
        (when (and overleaf--websocket (websocket-openp overleaf--websocket) overleaf--edit-queue)
          (let ((buf-string (overleaf--buffer-string (point-min) (point-max)))
                (next-version (1+ overleaf--doc-version))
                (edits (mapcar #'copy-sequence overleaf--edit-queue))
                (hash (overleaf--get-hash)))
            (setq-local buffer-read-only t)
            (overleaf--debug "======> FLUSH %S %i %S" (buffer-name) overleaf--doc-version overleaf--edit-queue)
            (overleaf--debug "Change context: %S" overleaf--change-context)
            (websocket-send-text
             overleaf--websocket
             (format "5:%i+::{\"name\":\"applyOtUpdate\",\"args\":[\"%s\",{\"doc\":\"%s\",\"op\":%s%s,\"v\":%i,\"lastV\":%i,\"hash\":\"%s\"}]}"
                     overleaf--sequence-id
                     overleaf-document-id
                     overleaf-document-id
                     (json-encode (apply #'vector overleaf--edit-queue))
                     (if overleaf-track-changes
                         (format ",\"meta\": {\"tc\":\"%s\"}"
                                 (overleaf--random-string 18))
                       "")
                     next-version
                     overleaf--doc-version
                     hash))

            (setq-local overleaf--sequence-id (1+ overleaf--sequence-id))
            (push
             (make-overleaf--update
              :from-version overleaf--doc-version
              :to-version next-version
              :edits edits
              :hash hash
              :buffer buf-string)
             overleaf--edits-in-flight)
            (overleaf--debug "send %S %i %i" (buffer-name) overleaf--doc-version next-version)
            (overleaf--reset-edit-queue)
            (overleaf--set-version next-version)
            (setq-local buffer-read-only nil)))))))

(defun overleaf--queue-edit (edit)
  "Add EDIT to the edit queue."
  (overleaf--debug "====> adding %s to queue" edit)
  (setq overleaf--edit-queue (nconc overleaf--edit-queue (list edit)))
  (setq-local overleaf--last-change-type nil)
  (setq-local overleaf--last-change-begin -1)
  (setq-local overleaf--last-change-end -1)
  (setq-local overleaf--deletion-buffer ""))

(defun overleaf-queue-pending-edit (&optional buffer)
  "Queue the edit in BUFFER that was in the process of being recorded.

See `overleaf--before-change-function' and `overleaf--after-change-function'."
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
              `(:p ,(- overleaf--last-change-begin 1) :i ,(overleaf--buffer-string overleaf--last-change-begin overleaf--last-change-end))))))))))

;;;; Cursors
(cl-defstruct overleaf--user-data
  "A container that holds information about a user on overleaf."
  id
  email
  name
  row
  column
  overlay)

(defvar-local overleaf--user-positions nil
  "A hash table relating user ids to overlays representing their cursors.")

(defun overleaf--id-to-color (id)
  "Hashes the ID to get a color to use for it.

Stolen from `rainbow-identifiers.el'."
  (let* ((hash (secure-hash 'sha1 id nil nil t))
         (len (length hash))
         (i 0)
         (result 0))
    (while (< i len)
      (setq result (+ (* result 256) (aref hash i)))
      (setq i (1+ i)))
    (aref overleaf-user-colors (mod result (length overleaf-user-colors)))))

(defvar overleaf--gravatar-cache (make-hash-table :test #'equal))

(defun overleaf--name-posframe-show (overlay)
  "Show a posframe showing the name corresponding to the OVERLAY."
  (when (and
         overlay
         (equal overleaf--buffer (window-buffer))
         (< (window-start) (overlay-start overlay))
         (> (window-end) (overlay-start overlay)))
    (let* ((buf (format "*overleaf-name-posframe %s*" overleaf-document-id))
           (text (overlay-get overlay 'name))
           (email (overlay-get overlay 'email))
           (gravatar (gethash email overleaf--gravatar-cache)))
      (when (and overleaf-cursor-gravatars
                 gravatar
                 (not (eq gravatar 'error)))
        (setq text (concat " " text))
        (put-text-property 0 1 'display gravatar text))
      (posframe-show buf
                     :string text
                     :timeout 1
                     :foreground-color "white"
                     :buffer overleaf--buffer
                     :background-color (overlay-get overlay 'color)
                     :position (save-excursion
                                 (goto-char (overlay-start overlay))
                                 (point))))))

(defun overleaf--row-col-to-pos (row column)
  "Translate ROW and COLUMN into a char position."
  (save-excursion
    (save-restriction
      (widen)
      (when row
        ;; Overleaf's 'row' is 0-indexed.  'count-lines' is 1-based.
        (if (> (1+ row) (count-lines (point-min) (point-max)))
            nil
          (goto-char (point-min))
          (forward-line row)
          (let ((pos (+ (point) column)))
            (if (> pos (point-max))
                nil
              pos)))))))

(defun overleaf--make-cursor-overlay (id name email row column)
  "Create a cursor overlay.

The overlay stores the ID, the NAME, the EMAIL of the user and is
displayed at line ROW and char COLUMN."
  (with-current-buffer overleaf--buffer
    (when-let* ((color (overleaf--id-to-color id))
                (pos (overleaf--row-col-to-pos row column)))
      (save-excursion
        (goto-char pos)
        (let* (;; this hack is necessary, as otherwise the font
               ;; property won't be applied
               (face `(:foreground "white" :background ,color))
               (overlay (make-overlay pos (1+ pos)))
               (show-name-fn (lambda (&rest _)
                               (overleaf--name-posframe-show overlay))))
          (overlay-put overlay 'face `(:foreground "white" :background ,color))
          (overlay-put overlay 'id id)
          (overlay-put overlay 'priority 1000)
          (overlay-put overlay 'name name)
          (overlay-put overlay 'email email)
          (overlay-put overlay 'help-echo name)
          (overlay-put overlay 'color color)
          (overlay-put overlay 'modification-hooks
                       (list show-name-fn))
          (overlay-put overlay 'insert-in-front-hooks
                       (list show-name-fn))
          (overlay-put overlay 'insert-in-behind-hooks
                       (list show-name-fn))
          (overlay-put overlay 'face face)

          overlay)))))

(defun overleaf--update-cursor (id name email row column)
  "Create or update a cursor overlay identified by ID.

The overlay stores the ID, the NAME, the EMAIL and is displayed
at line ROW and char COLUMN."
  (with-current-buffer overleaf--buffer
    (unless (equal id overleaf--user-id)
      (when (and overleaf-cursor-gravatars
                 (not (gethash email overleaf--gravatar-cache)))
        (gravatar-retrieve
         email
         (lambda (gravatar email)
           (puthash email gravatar overleaf--gravatar-cache))
         (list email)))
      (when-let* ((overlay
                   ;; recreating the overlay is necessary as otherwise the
                   ;; face properties won't be applied
                   (progn (overleaf--remove-cursor id)
                          (when-let* ((overlay (overleaf--make-cursor-overlay id name email row column)))
                            (puthash id overlay
                                     overleaf--user-positions))))
                  (newpos (overleaf--row-col-to-pos row column)))
        (overleaf--name-posframe-show overlay)))))

(defun overleaf--remove-cursor (id)
  "Remove the cursor with ID."
  (with-current-buffer overleaf--buffer
    (when-let* ((overlay (gethash id overleaf--user-positions))
                (pos (overlay-start overlay)))
      (delete-overlay overlay)
      (remhash id overleaf--user-positions)
      pos)))

;;;; Interface

(defun overleaf-goto-cursor ()
  "Go to the cursor position of another user."
  (interactive)
  (when (overleaf--connected-p)
    (let ((choices nil))
      (maphash
       (lambda (id overlay)
         (setq choices
               (cl-pushnew `(:fields
                             (,(overlay-get overlay 'name)
                              ,(overlay-get overlay 'email)
                              ,(overlay-get overlay 'id))
                             :data ,id
                             :propertize
                             (face
                              (:foreground ,(overlay-get overlay 'color))))
                           choices)))
       overleaf--user-positions)
      (goto-char
       (overlay-start
        (gethash
         (overleaf--completing-read "User: " choices)
         overleaf--user-positions))))))

(defun overleaf--other-users-p ()
  "Return non-nil if there are active remote users.
I.e., their cursor positions are known."
  (and overleaf--user-positions
       (< 0 (hash-table-count overleaf--user-positions))))

(defun overleaf--format-user-info (id overlay)
  "Return a formatted user info given by ID and OVERLAY.
Format these according to `overleaf-user-info-template'."
  (propertize
   (format-spec overleaf-user-info-template
                `((?n . ,(overlay-get overlay 'name))
                  (?e . ,(overlay-get overlay 'email))
                  (?i . ,id)
                  (?w . ,(if which-function-mode
                             (with-current-buffer (overlay-buffer overlay)
                               (save-excursion
                                 (save-restriction
                                   (widen)
                                   (goto-char (overlay-start overlay))
                                   (declare-function which-function "which-func")
                                   (or (which-function) ""))))
                           ""))))
   'face `(:foreground ,(overlay-get overlay 'color))))

(cl-defstruct (xref-overleaf-location
               (:constructor xref-make-overleaf-location (buffer position)))
  buffer position)

(cl-defmethod xref-location-line ((l xref-overleaf-location))
  "Return the line number corresponding to the location L."
  (pcase-let (((cl-struct xref-buffer-location buffer position) l))
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char position)
          (line-number-at-pos nil t))))))

(cl-defmethod xref-location-marker ((l xref-overleaf-location))
  "Make a marker at the location L."
  (pcase-let (((cl-struct xref-overleaf-location buffer position) l))
    (let ((m (make-marker)))
      (move-marker m position buffer))))

(cl-defmethod xref-location-group ((l xref-overleaf-location))
  "Get the group of the location L."
  (pcase-let (((cl-struct xref-overleaf-location buffer) l))
    (with-current-buffer buffer
      (format "%s (%s)"
              (or (buffer-file-name buffer)
                  (format "(buffer %s)" (buffer-name buffer)))
              overleaf-document-id))))

(defun overleaf-list-users ()
  "List other users' cursor positions in an xref buffer.
See variable `overleaf-user-info-template' for customization."
  (interactive)
  (let* ((xref-fn
          (lambda ()
            (let ((xrefs))
              (maphash (lambda (_ buff)
                         (message "%S" buff)
                         (with-current-buffer buff
                           (maphash
                            (lambda (id overlay)
                              (cl-pushnew
                               (xref-make
                                (overleaf--format-user-info id overlay)
                                (xref-make-overleaf-location (overlay-buffer overlay)
                                                             (overlay-start overlay)))
                               xrefs))
                            overleaf--user-positions)))
                       overleaf--ws-url->buffer-table)
              xrefs))))
    (if (overleaf--other-users-p)
        (xref-show-xrefs xref-fn nil)
      (overleaf--message "No other users edit this document."))))

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
   (let ((session (overleaf--webdriver-make-session)))
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
  "Choose a project and file interactively.

Optionally prompt for the overleaf server URL."
  (interactive
   (list
    (read-string "Overleaf URL: " (overleaf--url))))

  (require 'mm-url)
  (declare-function mm-url-decode-entities-string "mm-url")
  (overleaf-disconnect)
  (setq-local overleaf-url url)
  (setq-local overleaf-project-id nil)
  (setq-local overleaf-document-id nil)
  (let* ((cookies (overleaf--get-cookies))
         (project-page (plz 'get (format "%s/project" (overleaf--url))
                         :headers `(("Cookie" . ,cookies)
                                    ("Origin" . ,(overleaf--url)))))
         (projects (plist-get
                    (save-match-data
                      (string-match "name=\"ol-prefetchedProjectsBlob\".*?content=\"\\(.*?\\)\"" project-page)
                      (json-parse-string
                       (url-unhex-string (mm-url-decode-entities-string
                                          (match-string 1 project-page)))
                       :object-type 'plist :array-type 'list))
                    :projects))
         (collection (mapcar (lambda (project)
                               `(:fields
                                 (,(plist-get project :name)
                                  ,(plist-get (plist-get project :owner) :email))
                                 :data ,(plist-get project :id)))
                             projects)))
    (setq-local overleaf-project-id
                (overleaf--completing-read "Project: " collection))
    (setq-local overleaf-document-id nil)
    (overleaf-connect)))

;;;###autoload
(defun overleaf-connect ()
  "Connect current buffer to overleaf.

Requires `overleaf-cookies' to be set.  Prompts for the
`overleaf-project-id'.  The `overleaf-document-id' is prompted for
automatically.  Both these variables will be saved to the buffer."
  (interactive)

  (overleaf-mode t)
  (overleaf-disconnect)
  (if overleaf-cookies
      (let ((overleaf--buffer (current-buffer)))
        (with-current-buffer overleaf--buffer
          (unless overleaf-url
            (setq-local overleaf-url (read-string "Overleaf URL: " (overleaf--url))))
          (if overleaf-project-id
              (let* ((cookies (overleaf--get-cookies))
                     (response-cookie
                      (plz 'get (format "%s/socket.io/socket.io.js" (overleaf--url))
                        :as 'response
                        :headers `(("Cookie" . ,cookies)
                                   ("Origin" . ,(overleaf--url)))))


                     ;; this only seems to be necessary on the overleaf.com instance
                     (gclb-cookie
                      (let ((cookie (alist-get 'set-cookie (plz-response-headers response-cookie))))
                        (when cookie (save-match-data
                                       (string-match "\\(GCLB=.*?\\);" cookie)
                                       (match-string 1 cookie)))))
                     (full-cookies (format "%s; %s" cookies gclb-cookie))

                     (response
                      (plz 'get (format "%s/socket.io/1/?projectId=%s&esh=1&ssp=1" (overleaf--url) overleaf-project-id)
                        :as 'response
                        :headers `(("Cookie" . ,full-cookies)
                                   ("Origin" . ,(overleaf--url)))))
                     (ws-id
                      (car (string-split
                            (plz-response-body response) ":"))))

                (overleaf--debug "Connecting %s %s" overleaf-project-id overleaf-document-id)

                (setq-local overleaf--last-good-state nil)
                (setq-local overleaf--history '())
                (setq-local overleaf--recent-updates '())
                (setq-local overleaf--last-change-type nil)
                (setq-local overleaf--deletion-buffer "")
                (setq-local overleaf--edit-queue '())
                (setq-local overleaf--edits-in-flight nil)
                (setq-local overleaf--user-positions (make-hash-table :test #'equal))
                (setq-local buffer-read-only t)
                (setq-local overleaf--doc-version -2)
                (setq-local overleaf--user-id "")
                (setq-local overleaf--receiving nil)

                ;; magic value, don't ask me why
                (setq-local overleaf--sequence-id 2)

                (puthash
                 (websocket-url
                  (setq-local overleaf--websocket
                              (websocket-open
                               (replace-regexp-in-string
                                "http" "ws"
                                (replace-regexp-in-string
                                 "https" "wss"
                                 (format "%s/socket.io/1/websocket/%s?projectId=%s&esh=1&ssp=1"
                                         (overleaf--url) ws-id overleaf-project-id)))
                               :on-message #'overleaf--on-message
                               :on-close #'overleaf--on-close
                               :on-open #'overleaf--on-open
                               :custom-header-alist `(("Cookie" . ,full-cookies)
                                                      ("Origin" . ,(overleaf--url))))))
                 overleaf--buffer
                 overleaf--ws-url->buffer-table)
                (overleaf--update-modeline))
            (overleaf-find-file (overleaf--url)))))
    (error "Please set `overleaf-cookies'")))

;;;###autoload
(defun overleaf-disconnect ()
  "Disconnect from overleaf."
  (interactive)
  (when overleaf--websocket
    (overleaf--message "Disconnecting")
    (maphash
     (lambda (_ overlay)
       (delete-overlay overlay))
     overleaf--user-positions)
    (setq-local overleaf--force-close t)
    (setq-local overleaf--edit-queue '())
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
         (cond ((< overleaf--doc-version 0)
                (overleaf--mode-line-item (if overleaf-use-nerdfont "⟲" "...") "connecting"))
               ((zerop (length overleaf--edit-queue))
                (overleaf--mode-line-item (if overleaf-use-nerdfont "󰄭" "C") "connected\nno pending edits"))
               (t
                (overleaf--mode-line-item
                 (format "%i" (length overleaf--edit-queue))
                 "connected\nnumber of outgoing messages in the queue")))
         (if overleaf-track-changes
             (overleaf--mode-line-item " 󰑋" "track-changes: on")
           ""))
      (overleaf--mode-line-item (if overleaf-use-nerdfont "󰌸" "X") "not connected"))
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

;;;###autoload
(make-obsolete-variable 'overleaf-keymap-prefix
                        "bind `overleaf-command-map' directly (see README)."
                        "1.1.4")

;;;###autoload
(defvar-keymap overleaf-command-map
  "c" #'overleaf-connect
  "d" #'overleaf-disconnect
  "t" #'overleaf-toggle-track-changes
  "s" #'overleaf-toggle-auto-save
  "b" #'overleaf-browse-project
  "f" #'overleaf-find-file
  "g" #'overleaf-goto-cursor
  "l" #'overleaf-list-users)

;;;###autoload
(define-minor-mode overleaf-mode
  "Toggle Overleaf Connection mode.
Interactively with no argument, this command toggles the mode."

  :init-value nil
  :lighter nil ; Use `overleaf--mode-line' instead.

  (if overleaf-mode
      (overleaf--init)
    (setq-local overleaf--mode-line "")
    (force-mode-line-update t)
    (overleaf-disconnect)))


(provide 'overleaf)

;;; overleaf.el ends here
