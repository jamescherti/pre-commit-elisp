;;; pre-commit-elisp.el --- Pre-commit Elisp files -*- lexical-binding: t -*-

;; Copyright (C) 2025 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.5
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

;;; Require

(defun pre-commit-elisp-byte-compile (prefix use-tmp-files)
  "Byte-compile the files passed as arguments.
PREFIX is the prefix used for displaying messages.
USE-TMP-FILES compile in temporary files instead in the elisp file directory."
  (require 'bytecomp)
  (let ((root (vc-call-backend 'Git 'root default-directory)))
    (if (not root)
        (error
         "Unable to determine the Git root directory of %s"
         default-directory)
      (let ((default-directory (expand-file-name root)))
        ;; Load .dir-locals.el
        (put 'pre-commit-elisp-load-path 'safe-local-variable
             (lambda (_v) t))  ; accept any value
        (hack-dir-local-variables-non-file-buffer)

        ;; Recursively add other directories (DISABLED)
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
                        (make-temp-file (concat file-sans-ext "-")
                                        nil
                                        ".el"))))
        (unwind-protect
            (progn
              ;; Add the file's directory to load-path
              (when (not (member dir load-path))
                (message "%sAdd to load-path: %s" prefix dir)
                (push dir load-path))

              (when tmpfile
                (copy-file file tmpfile t))

              (let ((default-directory dir))
                (if (byte-compile-file (if tmpfile
                                           tmpfile
                                         file))
                    (message "%sSuccess: %s" prefix file)
                  (setq failure t)
                  (message
                   "%sFailure: %s" prefix file))))
          (when tmpfile
            (when (file-exists-p tmpfile)
              (ignore-errors
                (delete-file tmpfile)))
            (let ((dest (funcall (if (bound-and-true-p
                                      byte-compile-dest-file-function)
                                     byte-compile-dest-file-function
                                   #'byte-compile-dest-file)
                                 tmpfile)))
              (when (and dest
                         (file-exists-p dest))
                (ignore-errors
                  (delete-file dest))))))))
    (when failure
      (kill-emacs 1))))

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
        (forward-line 1))

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

;;; pre-commit-elisp.el ends here
