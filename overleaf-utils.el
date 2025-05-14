;;; overleaf-utils.el --- Some utility functions for overleaf-connection.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions that would clutter the main source of overleaf-connection.el

;;; Code:
(require 'overleaf-vars)

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
  "Return the domain for which the cookies will be valid from the current value of `overleaf-url'."
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
    (mapconcat 'char-to-string xvec)))


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

(defmacro overleaf--warn (&rest args)
  "Print a warning message passing ARGS on to `display-warning'."
  `(display-warning 'overleaf (format ,@args)))

(defmacro overleaf--message (string &rest args)
  "Print a message with format string STRING and arguments ARGS."
  `(message  (format ,(concat "Overleaf: " string) ,@args)))


;;;; Webdriver
(defmacro overleaf--with-webdriver (&rest body)
  "Execute BODY if geckodriver is found and show an error message otherwise."
  `(if (not (executable-find "geckodriver"))
       (message-box "Please install geckodriver or set the cookies / document and project id manually.")
     ,@body))

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

(cl-defmacro overleaf--webdriver-wait-until-appears
    ((session xpath &optional (element-sym '_unused) (delay .1)) &rest body)
  "Wait until an element matching XPATH is found in SESSION, bind it to ELEMENT-SYM and execute BODY."
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


(provide 'overleaf-utils)
;;; overleaf-utils.el ends here
