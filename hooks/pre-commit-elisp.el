;;; pre-commit-elisp.el --- Pre-commit Elisp files -*- lexical-binding: t -*-

;; Copyright (C) 2025-2026 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti <https://www.jamescherti.com/contact/>
;; Version: 1.0.7
;; URL: https://github.com/jamescherti/pre-commit-elisp
;; Keywords: tools, convenience
;; Package-Requires: ((emacs "26.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Lisp code that recursively byte-compile and native-compile .el files.

;;; Code:

(defvar pre-commit-elisp-debug nil)

(defvar pre-commit-elisp-error-on-compile-warning nil)
(defvar pre-commit-elisp-load-path nil)

(defun pre-commit-elisp-byte-compile (prefix use-tmp-files)
  "Byte-compile the files passed as arguments.
PREFIX is the prefix used for displaying messages.
USE-TMP-FILES compile in temporary files instead in the elisp file directory."
  (pre-commit-elisp--compile prefix use-tmp-files 'byte))

(defun pre-commit-elisp--compile (prefix use-tmp-files compile-type)
  "Internal function to compile files using COMPILE-TYPE (\='byte or \='native).
PREFIX is the prefix used for displaying messages.
USE-TMP-FILES compile in temporary files instead in the elisp file directory."
  (cond
   ((eq compile-type 'native)
    (progn
      (require 'comp nil t)
      (unless (and (fboundp 'native-comp-available-p)
                   (native-comp-available-p))
        (error "Native compilation is not available"))))

   ((eq compile-type 'byte)
    (require 'bytecomp))

   (t
    (error "pre-commit-elisp--compile: `compile-type' has to be 'byte or 'native")))

  (let ((root (vc-call-backend 'Git 'root default-directory)))
    (if (not root)
        (error "Unable to determine the Git root directory of %s"
               default-directory)
      (when pre-commit-elisp-debug
        (message "[DEBUG] Root directory: %s" root))
      (let ((default-directory (expand-file-name root)))
        ;; Load .dir-locals.el
        ;; (setq enable-local-variables :safe)

        (put 'pre-commit-elisp-error-on-compile-warning 'safe-local-variable #'booleanp)
        (put 'pre-commit-elisp-load-path 'safe-local-variable #'listp)
        (with-temp-buffer
          (let ((enable-local-variables :safe))
            (hack-dir-local-variables-non-file-buffer))

          ;; Manually apply the alist to the global state
          (when pre-commit-elisp-debug
            (message "[DEBUG] file-local-variables-alist: %S"
                     file-local-variables-alist))
          (dolist (entry file-local-variables-alist)
            (let ((var (car entry))
                  (val (cdr entry)))
              (if (memq var '(pre-commit-elisp-error-on-compile-warning
                              pre-commit-elisp-load-path))
                  (progn
                    (set-default var val)

                    (cond

                     ((and (eq var 'pre-commit-elisp-load-path)
                           (not (listp pre-commit-elisp-load-path)))
                      (message
                       "Error: The pre-commit-elisp-load-path variable has to be a list")
                      (kill-emacs 1))


                     ((and (eq var 'pre-commit-elisp-error-on-compile-warning)
                           (not (booleanp pre-commit-elisp-error-on-compile-warning)))
                      (message
                       "Error: The pre-commit-elisp-error-on-compile-warning variable has to be a boolean")
                      (kill-emacs 1)))

                    (when pre-commit-elisp-debug
                      (message "[DEBUG] %sApplied dir-local: %S = %s"
                               prefix var val)))
                (when pre-commit-elisp-debug
                  (message "[DEBUG] IGNORE: %s" var))))))

        (when pre-commit-elisp-debug
          (message "[DEBUG] VARIABLES:")
          (message "  - pre-commit-elisp-load-path: %S"
                   (when (boundp 'pre-commit-elisp-load-path)
                     pre-commit-elisp-load-path))
          (message "  - pre-commit-elisp-error-on-compile-warning: %S"
                   (when (boundp 'pre-commit-elisp-error-on-compile-warning)
                     pre-commit-elisp-error-on-compile-warning)))

        ;; Recursively add other directories
        (let ((pre-commit-elisp-load-path
               (if (boundp 'pre-commit-elisp-load-path)
                   pre-commit-elisp-load-path
                 (list "."))))
          (dolist (dir pre-commit-elisp-load-path)
            (let ((default-directory (file-name-as-directory
                                      (expand-file-name dir))))
              ;; Push the current directory to load path
              (if (string-suffix-p "/" dir)
                  (progn
                    (message "%s%s%s"
                             prefix
                             "Add recursively to load-path: "
                             default-directory)
                    (normal-top-level-add-subdirs-to-load-path))
                (progn
                  (message "%sAdd to load-path: %s" prefix default-directory)
                  (push default-directory load-path)))))))))

  (let ((failure nil)
        (byte-compile-warnings t)  ; Strict mode
        (original-load-path (copy-sequence load-path)))
    (dolist (file command-line-args-left)
      (setq file (expand-file-name file))
      (let* ((dir (file-name-as-directory (file-name-directory file)))
             (load-path (copy-sequence original-load-path))
             (file-sans-ext (file-name-sans-extension
                             (file-name-nondirectory file)))
             (tmpfile (when use-tmp-files
                        (cond
                         ((eq compile-type 'native)
                          (make-temp-file (concat file-sans-ext "-")
                                          nil
                                          ".eln"))

                         ((eq compile-type 'byte)
                          (make-temp-file (concat file-sans-ext "-")
                                          nil
                                          ".el")))))
             (compiled-dest nil))

        ;; Add the file's directory to load-path
        (when (not (member dir load-path))
          (message "%sAdd to load-path: %s" prefix dir)
          (push dir load-path))

        (let ((default-directory dir))
          (cond
           ((eq compile-type 'byte)
            ;; Byte-compile
            (let ((el-file (if tmpfile tmpfile file))
                  (delete-tmpfile (when tmpfile t)))
              (when tmpfile
                (copy-file file tmpfile t))

              (unwind-protect
                  (progn
                    (message "[INFO] Byte compile: %s" el-file)
                    (let ((byte-compile-error-on-warn
                           (bound-and-true-p
                            pre-commit-elisp-error-on-compile-warning)))
                      (condition-case err
                          (setq failure
                                (let ((result (byte-compile-file el-file)))
                                  (if (eq result 'no-byte-compile)
                                      nil
                                    (not result))))
                        (error
                         (message "%s" (error-message-string err))
                         (setq failure t)
                         nil)))

                    (if failure
                        (message "%sFailure: %s" prefix file)
                      (message "%sSuccess: %s" prefix file)))
                ;; Unwind protect
                (when delete-tmpfile
                  (when pre-commit-elisp-debug
                    (message "[DEBUG] Delete: %s" tmpfile))
                  (ignore-errors
                    (delete-file tmpfile))))))

           ;; Native compile
           ((eq compile-type 'native)
            (if (fboundp 'native-compile)
                (progn
                  (let ((dest-file tmpfile))
                    (unwind-protect
                        (progn
                          (message "[INFO] Native compile: %s -> %s"
                                   file dest-file)
                          (setq compiled-dest
                                (let ((byte-compile-error-on-warn
                                       (bound-and-true-p
                                        pre-commit-elisp-error-on-compile-warning)))
                                  (condition-case err
                                      (native-compile file dest-file)
                                    (error
                                     (message "%s" (error-message-string err))
                                     (setq failure t)
                                     nil)))))
                      (when (and dest-file (file-exists-p dest-file))
                        (when pre-commit-elisp-debug
                          (message "[DEBUG] Delete: %s" dest-file))
                        (ignore-errors
                          (delete-file dest-file))))))
              (error "Undefined function: native-compile"))

            (if compiled-dest
                (message "%sSuccess: %s" prefix file)
              (setq failure t)
              (message "%sFailure: %s" prefix file)))))))
    (when failure
      (kill-emacs 1))))

(defun pre-commit-elisp-native-compile (prefix use-tmp-files)
  "Native-compile the files passed as arguments.
PREFIX is the prefix used for displaying messages.
USE-TMP-FILES compile in temporary files instead in the elisp file directory."
  (setq native-comp-deferred-compilation nil)
  (setq native-comp-jit-compilation nil)
  (pre-commit-elisp--compile prefix use-tmp-files 'native))

(defun pre-commit-elisp-indent ()
  "Indent the Elisp files passed as command-line arguments."
  (dolist (file command-line-args-left)
    (message "[ELISP INDENT] %s" file)
    (with-temp-buffer
      (put 'cl-letf 'lisp-indent-function 1)
      (put 'lightemacs-use-package 'lisp-indent-function 0)
      (put 'lightemacs-define-keybindings 'lisp-indent-function 1)
      (put 'lightemacs-verbose-message 'lisp-indent-function 0)
      (put 'lightemacs-save-window-hscroll 'lisp-indent-function 0)
      (put 'lightemacs-define-mode-add-hook-to 'lisp-indent-function 0)
      (put 'lightemacs-save-window-start 'lisp-indent-function 0)

      ;; Modify settings
      (setq-local lexical-binding t)
      (insert-file-contents file)
      (emacs-lisp-mode)
      (check-parens)

      ;; Remove tabs
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-beg (line-beginning-position)))
          (goto-char line-beg)
          (skip-chars-forward " \t")
          (let ((indent-end (point)))
            (when (> indent-end line-beg)
              (untabify line-beg indent-end))))
        (if (= (forward-line 1) 1)
            (goto-char (point-max))))

      ;; Reindent
      (let ((beg (point-min))
            (end (point-max)))
        (save-restriction
          (narrow-to-region beg end)
          (if (save-excursion
                (goto-char beg)
                (= end (line-beginning-position 2)))
              (indent-according-to-mode)
            (goto-char beg)
            (indent-region beg end))))

      (write-region (point-min) (point-max) file))))

(provide 'pre-commit-elisp)

;; Local variables:
;; byte-compile-warnings: (not obsolete free-vars)
;; End:

;;; pre-commit-elisp.el ends here
