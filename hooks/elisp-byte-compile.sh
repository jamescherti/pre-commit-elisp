#!/usr/bin/env sh
#
# Author: James Cherti
# URL: https://github.com/jamescherti/pre-commit-elisp
#
# Description:
# ------------
# Byte-compile Elisp files to detect compilation errors.
#
# Alternative approach: To prevent .elc files from appearing in the repository,
# use `elisp-check-byte-compile` instead of `elisp-byte-compile`. It performs
# compilation checks entirely in temporary files.
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

     ;; Add the root directory of the Git repository to load-path
     (let ((root (vc-call-backend 'Git 'root default-directory)))
       (if (not root)
           (error
            \"Unable to determine the Git root directory of %s\"
            default-directory)
         (let ((default-directory (expand-file-name root)))
           ;; Load .dir-locals.el
           (put 'pre-commit-elisp-load-path 'safe-local-variable
                (lambda (v) t))  ; accept any value
           (hack-dir-local-variables-non-file-buffer)

           ;; Recursively add other directories (DISABLED)
           (unless (boundp 'pre-commit-elisp-load-path)
             (setq pre-commit-elisp-load-path (list \".\")))

           (dolist (dir pre-commit-elisp-load-path)
             (let ((default-directory (file-name-as-directory
                                       (expand-file-name dir))))
               ;; Push the current directory to load path
               (if (string-suffix-p \"/\" dir)
                   (progn
                     (message \"%s%s%s\"
                              \"[ELISP CHECK-BYTE-COMPILE] \"
                              \"Add recursively to load-path: \"
                              default-directory)
                     (normal-top-level-add-subdirs-to-load-path))
                 (progn
                   (message
                    \"[ELISP CHECK-BYTE-COMPILE] Add to load-path: %s\"
                    default-directory)
                   (push default-directory load-path))))))))

     (setq byte-compile-warnings t)  ; Strict mode
     ;; TODO: check before adding
     (push (expand-file-name \".\") load-path)
     (let ((failure nil)
           (original-load-path (copy-sequence load-path)))
       (dolist (file command-line-args-left)
         (setq file (expand-file-name file))
         (let ((dir (file-name-directory file))
               (load-path (copy-sequence original-load-path)))
           (message \"[ELISP BYTE-COMPILE] Add to load-path: %s\"
                    dir)
           (push dir load-path)

           (let ((default-directory dir))
             (if (byte-compile-file file)
                 (message \"[ELISP BYTE-COMPILE] Success: %s\" file)
               (setq failure t)
               (message \"[ELISP BYTE-COMPILE] Failure: %s\" file)))))
       (when failure
         (kill-emacs 1))))" \
  "$@"
