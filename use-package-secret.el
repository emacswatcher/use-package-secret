;;; use-package-secret.el --- configure per-user, non-customizeable vars in use-package

;; Copyright (C)  2018 Emacs Watcher

;; Author: Emacs Watcher
;; Created: ew (2018-04-18):
;; Version: 0.21

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

;; Idiomatically configure symbols via external files in
;; use-package. Provides support for the :secret keyword. It
;; supports the following arguments:

;; * In the init file:

;; 1. The basic form

;; :secret variable ...

;; - The variable should be unquoted.
;; - A filename will be generated from the package name and variable name.

;; 2. :File keyword

;; :secret [ ( :file . absolute-filename ) ] [ variable ] ...

;; By default, secrets are stored in
;; {$user-emacs-dir}/{$package-name}/secret.el

;; In order to change the default, add a consp whose car is the
;; keyword :file and whose cdr is an absolute path to the file.

;; 3. :Must and :may keywords

;; :secret [ :must ] [ variable ] ...

;; :secret [ :may ] [ variable ] ...

;; ':Must' and ’:may’ are on and off switches which control the same
;; internal processing state. When the state is ':must', an error will
;; be signalled by use-package if the variable cannot be set.
;; ':May'-decorated forms will fail silently if the file is not
;; present. The default state is ':may'.

;; There is also a customizable boolean
;; 'use-package-load-seq-mustify-mays' which will force all forms to
;; act as though they are ':must'-decorated.

;; 4. :Verbose keyword

;; :secret :verbose [ variable ]

;; The :verbose decorator enables extra logging.

;; There is also a customizable boolean
;; ’use-package-secret-verbose’ which enables extra logging globally.

;; 5. Lists

;; Organizing the parameters into lists provides a scope for all the
;; keyword decorators. A new context resets the decorated states to their
;; post-customization states.

;; :secret (:must tom dick :verbose harry ) :verbose diane

;; In this example, ':must' applies to tom, dick and harry. Harry will
;; have ':verbose' logging. diane will be processed with ':may' and
;; ':verbose'.

;; * In the secret file:

;; A secret file is a plist. The keys are symbol names and the values
;; are the settings. Values are NOT evaluated when loaded.

;; Sample:

;; (
;;  symbol-1 '("sample" "list" 1)
;;  symbol-2 "sample-string-literal"
;;  symbol-3 1024)

;; Files are loaded once, on first use. Once a given file has been
;; loaded, it will not be loaded again, even in the context of a
;; nested list.

;;; Code:

(require 'use-package-core)

(defconst use-package-secret-id "secret: ")

(defgroup use-package-secret nil
  "Provide a use-package keyword to configure non-custom variables."
  :group 'use-package
  :prefix "use-package-secret-"
  :version "0.2")

(defcustom use-package-secret-verbose nil
  "If non-nil, log everything."
  :type 'boolean
  :group 'use-package-secret
  )

(defcustom use-package-secret-default-filename-fn nil
  "Given package-name, return absolute path to secret file."
  :type 'function
  :group 'use-package-secret
  )

(defcustom use-package-secret-mustify-mays nil
  "If non-nil, treat all ’may’ arguments as ’must’.

 If nil, only log when configuration says I :must."
  :type 'boolean
  :group 'use-package-secret
  )

(defsubst use-package-secret-message (format-str &rest args)
  "Wrap FORMAT-STR and ARGS ’message’ parameters for consistency."
  (apply #'message
         (append (list (concat use-package-secret-id format-str)) args)))

(defsubst use-package-secret-error (format-str &rest args)
  "Wrap FORMAT-STR and ARGS ’message’ parameters for consistency."
  (apply #'use-package-error
         (append (list (concat use-package-secret-id format-str)) args)))

(defsubst use-package-secret-normalization-msg (name label args)
  "Wrap standard message for NAME LABEL ARGS."
  (use-package-secret-message "normalizing %s %s %s" name label args))

(defun use-package-secret-default-filename (name verbose)
  "Make filename for NAME / VARIABLE and optionally be VERBOSE about it."
  (let ((filename (if (functionp use-package-secret-default-filename-fn)
                      (funcall use-package-secret-default-filename-fn
                               (use-package-as-string name))
                    (convert-standard-filename
                     (concat user-emacs-directory
                             (use-package-as-string name)
                             "/secret.el")))))
    (when verbose
      (use-package-secret-message
       "secrets file for %s set to %s" name filename))
    filename))

(defun use-package-secret-normalize (name label args &optional recurse)
  "Normalize values for package NAME LABEL, and ARGS, possibly RECURSE.

Normal form is (variable)"
  (when use-package-secret-verbose
    (use-package-secret-normalization-msg name label args))
  (let* ((arg (if (listp args)
                  args
                (list args)))
         (must nil)
         (verbose use-package-secret-verbose)
         (mustify use-package-secret-mustify-mays)
         (file (use-package-secret-default-filename name verbose))
         (args* nil))
    (while arg
      (let ((x (car arg)))
        (cond
         ((use-package-non-nil-symbolp x)
          (setq args* (push
                       (list x
                             file
                             must
                             verbose)
                       args*))
          (setq arg (cdr arg)))
         ((consp x)
          (cond
           ((eq (car x) :file)
            (let ((y (cdr x)))
              (if (and (or must
                           mustify)
                       (not (file-readable-p y)))
                  (use-package-secret-error "%s must be a readable file" y)
                (progn
                  (setq file y)
                  (setq arg (cdr arg))
                  (when verbose
                    (use-package-secret-message
                     "secret file for %s set to %s"
                     (use-package-as-string name) y))
                  )
                ))
            )
           (t (use-package-secret-error "%s/%s misconfiguration at ’%s’"
                                        (use-package-as-string name)
                                        (use-package-as-string label)
                                        (car x)))))
         ((eq x :verbose)
          (setq verbose t
                arg (cdr arg))
          (when verbose
            (use-package-secret-message "set verbose logging for %s" name))
          )
         ((eq x :must)
          (setq must t
                arg (cdr arg))
          (when verbose
            (use-package-secret-message "set ’must’ processing for %s" name)))
         ((eq x :may)
          (setq must nil
                arg (cdr arg))
          (when verbose
            (use-package-secret-message "set ’may’ processing for %s" name)))

         ((listp x)
          (setq args*
                (push (use-package-secret-normalize name label x) args* t))
          (setq arg (cdr arg)))
         (t
          (use-package-secret-error
           (format "%s/%s misconfiguration at ’%s’"
                   (use-package-as-string name)
                   (use-package-as-string label)
                   (use-package-as-string x)))))))
    (nreverse args*)))

;;;###autoload
(defun use-package-normalize/:secret (name keyword args)
  "Pass package NAME, KEYWORD being processed and any ARGS to normalizer."
  (when use-package-secret-verbose
    (use-package-secret-message "normalize api %s %s %s" name keyword args))
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-secret-normalize name) t))

(defun use-package-secret-load (file)
  "Evaluate FILE and return the result, failing silently."
  (ignore-errors
    (read-from-whole-string
     (with-temp-buffer
       (insert-file-contents file)
       (buffer-string)))))

(defun use-package-secret-process (variable file must verbose)
  "Set VARIABLE loaded from PACKAGE plist file, log if MUST or VERBOSE.

If FILE does not exist or is malformed, fail silently."
  (let ((sym (format "use-package-secret-%s" file)))
    (if (not (intern-soft sym))
        (if (file-exists-p file)
            (setplist (intern sym) (use-package-secret-load file))
          (when (or must
                    verbose)
            (use-package-secret-message "file %s not found" file))))
    (when (intern-soft sym)
      (let* ((isym (intern-soft sym))
             (ivar (intern-soft variable))
             (ival (get isym ivar)))
        (set ivar ival)
        (when verbose
          (use-package-secret-message "set %s to %s" ivar ival))))))

;;;###autoload
(defun use-package-handler/:secret (name keyword args rest state)
  "NAME KEYWORD ARGS REST STATE."
  (use-package-concat
   (mapcar #'(lambda (var)
               (apply #'use-package-secret-process `,@var))
           args)
   (use-package-process-keywords name rest state)))

(let ((pos (+ 1 (cl-position :custom use-package-keywords))))
  (setq use-package-keywords (nconc (subseq use-package-keywords 0 pos)
                                    (list :secret)
                                    (nthcdr (+ 1 pos) use-package-keywords))))

(provide 'use-package-secret)

;;; use-package-secret.el ends here
