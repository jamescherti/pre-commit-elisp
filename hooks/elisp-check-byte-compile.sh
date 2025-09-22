#!/usr/bin/env sh
#
# Author: James Cherti
# URL: https://github.com/jamescherti/pre-commit-elisp
#
# Description:
# ------------
# This script byte-compiles the Emacs Lisp files provided as arguments into
# temporary files. All generated temporary files, including the compiled .elc
# files, are automatically deleted after compilation to ensure no residual
# artifacts remain.
#
# License:
# --------
# Copyright (C) 2025 James Cherti
#
# Distributed under terms of the GNU General Public License version 3.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

exec emacs --batch --eval \
  "(with-temp-buffer
     (setq-local lexical-binding t)
     (require 'bytecomp nil t)

     ;; Add the root directory of the Git repository to load-path
     (let ((root (vc-call-backend 'Git 'root default-directory)))
       (if (not root)
           (error
            \"Unable to determine the Git root directory of %s\"
            default-directory)
         (let ((default-directory (expand-file-name root)))
           ;; Push the current directory to load path
           (message \"[ELISP CHECK-BYTE-COMPILE] Add to load-path: %s\"
                    default-directory)
           (push default-directory load-path)

           ;; Recursively add other directories (DISABLED)
           ;; TODO: Add an option for this
           ;; (message
           ;;   \"%s%s\"
           ;;   \"[ELISP CHECK-BYTE-COMPILE] \"
           ;;   \"Add recursively to load-path: \"
           ;;   default-directory)
           ;; (normal-top-level-add-subdirs-to-load-path)
           )))

     (let ((failure nil)
           (byte-compile-warnings t)  ; Strict mode
           (original-load-path (copy-sequence load-path)))

       (dolist (file command-line-args-left)
         (setq file (expand-file-name file))
         (let* ((dir (file-name-directory file))
                (load-path (copy-sequence original-load-path))
                (file-sans-ext (file-name-sans-extension
                                (file-name-nondirectory file)))
                (tmpfile (make-temp-file (concat file-sans-ext \"-\")
                                         nil
                                         \".el\")))
           (unwind-protect
               (progn
                 ;; Add the file's directory to load-path
                 (message \"[ELISP CHECK-BYTE-COMPILE] Add to load-path: %s\"
                          dir)
                 (push dir load-path)

                 (copy-file file tmpfile t)
                 (let ((default-directory dir))
                   (if (byte-compile-file tmpfile)
                       (message \"[ELISP CHECK-BYTE-COMPILE] Success: %s\" file)
                     (setq failure t)
                     (message \"[ELISP CHECK-BYTE-COMPILE] Failure: %s\" file))))
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
                   (delete-file dest)))))))
       (when failure
         (kill-emacs 1))))" \
  "$@"
