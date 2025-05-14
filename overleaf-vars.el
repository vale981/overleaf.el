;;; overleaf-vars.el --- Shared variable and customize definitions  -*- lexical-binding: t; -*-

;;; Commentary:
;; Variable definitions that are shared across the project.

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
cookies from a gpg encrypted file. See `overleaf-read-cookies-from-file'.

The cookies are most easily obtained from the developer tools in the
browser.")

(defun overleaf-read-cookies-from-file (file)
  "Return a cookie saving function to load the cookie-string from FILE.
To be used with `overleaf-cookies'."
  #'(lambda ()
      (with-temp-buffer
        (insert-file-contents (expand-file-name file))
        (string-trim (buffer-string)))))

(defvar overleaf-save-cookies #'(lambda (cookies)
                                  (setq overleaf-cookies cookies))
  "A function (lambda) that stores the session cookies.
The function receives a string containing the session cookies and stores
in a way that `overleaf-cookies' can access it.  The default
implementation simply sets `overleaf-cookies' to the string value.
Another possibility is to store them into a gpg encrypted file.  See
`overleaf-save-cookies-to-file'.")

(defun overleaf-save-cookies-to-file (file)
  "Return a cookie saving function to save the cookie-string to FILE.
To be used with `overleaf-save-cookies'."
  #'(lambda (cookies)
      (with-temp-file file
        (insert cookies))))

(defcustom overleaf-url "https://www.overleaf.com"
  "The url of the overleaf server."
  :type 'string
  :group 'overleaf-connection-mode)

(defcustom overleaf-flush-interval .5
  "The idle-timer delay to flush the edit queue."
  :type 'float
  :group 'overleaf-connection-mode)

(defcustom overleaf-message-interval .5
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

(defvar-local overleaf--mode-line ""
  "Contents of the mode-line indicator.")

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
Should only contain known-good states.  Is limited to length `overleaf-history-buffer-length'.")

(defvar-local overleaf-history-buffer-length 50
  "How many past versions of the buffer to keep in memory.")

(defvar-local overleaf--recent-updates nil
  "An alist that contains the `overleaf-update-buffer-length' recent updates.

It has elements of the form `((from-version . (to-version . update)) ...)'.")

(defvar-local overleaf-update-buffer-length 50
  "How many past updates of the buffer to keep in memory.")

(defvar overleaf--current-cookies nil
  "The current cookies so we don't have to read them every time.")

(defvar-local overleaf--receiving nil
  "When t we are currently in the process of receiving and processing an update.")

(cl-defstruct overleaf--queued-message
  "A container that holds a queued message."
  sequence-id
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
HASH is a hash obtained from `overleaf--get-hash' after the edit is performed.
BUFFER is the buffer value after applying the update."
  edits
  from-version
  to-version
  hash
  buffer)


(provide 'overleaf-vars)
;;; overleaf-vars.el ends here
