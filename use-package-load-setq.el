;;; use-package-load-setq.el --- configure per-user, non-customizeable vars in use-package

;; Copyright (C)  2018 Emacs Watcher

;; Author: Emacs Watcher
;; Created: ew (2018-04-18):
;; Version: 0.2

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Idiomatically configure per-user, non-customizeable vars in
;; use-package. Provides support for the :load-setq keyword. It
;; supports the following arguments:

;; 1. The basic form

;; :load-setq variable ...

;; - The variable should be unquoted.
;; - A filename will be generated from the package name and variable name.

;; 2. :Must and :may decorators

;; :load-setq [ :must ] [ variable ] ...

;; :load-setq [ :may ] [ variable ] ...

;; ':Must' and ’:may’ are on and off switches which control the same
;; internal processing state. When the state is ':must', an error will
;; be signalled by use-package if the variable cannot be set.
;; ':May'-decorated forms will fail silently if the file is not
;; present. The default state is ':may'.

;; There is also a customizable boolean
;; 'use-package-load-seq-mustify-mays' which will force all forms to
;; act as though they are ':must'-decorated.

;; 3. :Verbose decorator

;; :load-setq :verbose [ variable ]

;; The :verbose decorator enables extra logging.

;; There is also a customizable boolean
;; ’use-package-load-setq-verbose’ which enables extra logging globally.

;; 4. Lists

;; Organizing the parameters into lists provides a context for all the
;; decorators. A new context resets the decorated states to their
;; defaults, i.e. ':may' and no extra logging.

;; :load-setq (:must tom dick :verbose harry ) :verbose diane

;; In this example, ':must' applies to tom, dick and harry. Harry will
;; have ':verbose' logging. diane will be processed with ':may' and
;; ':verbose'.

;;; Code:

(require 'use-package-core)

(defconst upls-id "load-setq: ")

(defgroup use-package-load-setq nil
  "Provide a use-package keyword to configure non-custom variables."
  :group 'use-package
  :prefix "use-package-load-setq-"
  :version "0.2")

(defcustom use-package-load-setq-verbose nil
  "If non-nil, log everything."
  :type 'boolean
  :group 'use-package-load-setq
  )

(defcustom use-package-load-setq-mustify-mays nil
  "If non-nil, treat all ’may’ arguments as ’must’.

 If nil, only log when configuration says I :must."
  :type 'boolean
  :group 'use-package-load-setq
  )

(defsubst upls-message (format-str &rest args)
  "Wrap FORMAT-STR and ARGS ’message’ parameters for consistency."
  (apply #'message (append (list (concat upls-id format-str)) args)))

(defsubst upls-error (format-str &rest args)
  "Wrap FORMAT-STR and ARGS ’message’ parameters for consistency."
  (apply #'use-package-error (append (list (concat upls-id format-str)) args)))

(defsubst upls-normalization-msg (name label args)
  "Wrap standard message for NAME LABEL ARGS."
  (upls-message "normalizing %s %s %s" name label args))

(defun upls-default-filename (name verbose)
  "Make filename for NAME / VARIABLE and optionally be VERBOSE about it."
  (let ((filename (convert-standard-filename
                   (concat user-emacs-directory
                           (use-package-as-string name)
                           "/load-setq.el"))))
    (when verbose
      (upls-message "filename for %s is %s" variable filename))
    filename))

(defun upls-normalize (name label args &optional recurse)
  "Normalize values for package NAME LABEL, and ARGS, possibly RECURSE.

Normal form is (variable)"
  (when use-package-load-setq-verbose
    (upls-normalization-msg name label args))
  (let ((arg (unless (listp args)
               (list args)))
        (must nil)
        (verbose use-package-load-setq-verbose)
        (mustify use-package-load-setq-mustify-mays)
        (args* nil))
    (while arg
      (let ((x (car arg)))
        (cond
         ((use-package-non-nil-symbolp x)
          (setq args* (push
                       (list x
                             name
                             must
                             verbose)
                       args*))
          (setq arg (cdr arg)))

         ((eq x :verbose)
          (setq verbose t
                arg (cdr arg))
          (when verbose
            (upls-message "set verbose logging for %s" name))
          )
         ((eq x :must)
          (setq must t
                arg (cdr arg))
          (when verbose
            (upls-message "set ’must’ processing for %s" name)))
         ((eq x :may)
          (setq must nil
                arg (cdr arg))
          (when verbose
            (upls-message "set ’may’ processing for %s" name)))

         ((listp x)
          (setq args*
                (push (upls-normalize name label x) args*))
          (setq arg (cdr arg)))
         ((and (consp x)
               (or must
                   mustify)
               (not (file-readable-p (cdr x))))
          (upls-error
           (format "%s/%s %s must be a readable file"
                   (use-package-as-string name)
                   (use-package-as-string label)
                   (cdr x))))
         (t
          (upls-error
           (format "%s/%s misconfiguration at ’%s’"
                   (use-package-as-string name)
                   (use-package-as-string label)
                   (use-package-as-string x)))))))
    (nreverse args*)))

;;;###autoload
(defun use-package-normalize/:load-setq (name keyword args)
  "Pass package NAME, KEYWORD being processed and any ARGS to normalizer."
  (when use-package-load-setq-verbose
    (upls-message "normalize api %s %s %s" name keyword args))
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'upls-normalize name) t))

(defun upls-load (file)
  "Evaluate FILE and return the result, failing silently."
  (eval
   (ignore-errors
     (read-from-whole-string
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string))))))

(defun upls-process (variable package must verbose)
  "Set VARIABLE loaded from PACKAGE plist file, log if MUST or VERBOSE.

If FILE does not exist, fail silently."
  (let ((sym (format "use-package-load-setq-%s" package))
        (file (upls-default-filename package)))
    (if (file-exists-p file)
        (progn
          (setplist sym (upls-load file))
          (setq variable (get sym variable)))
      (when (or must
                verbose)
        (upls-message "file %s not found" file)))))

;;;###autoload
(defun use-package-handler/:load-setq (name keyword arg rest state)
  "NAME KEYWORD ARG REST STATE."
  (when use-package-load-setq-verbose
    (upls-message "use-package handler api (%s %s %s %s %s)"
                  name keyword arg rest state))
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     (mapcar #'(lambda (var)
                 '(cl-destructuring-bind (variable file must verbose) ,var
                    (upls-process variable file must verbose)))
             arg)
     body)))

(let ((pos (+ 1 (cl-position :custom use-package-keywords))))
  (setq use-package-keywords (nconc (subseq use-package-keywords 0 pos)
                                    (list :load-setq)
                                    (nthcdr (+ 1 pos) use-package-keywords))))

(provide 'use-package-load-setq)

;;; use-package-load-setq.el ends here
