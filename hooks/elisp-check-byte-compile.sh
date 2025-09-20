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
  "(progn
     (require 'bytecomp)
     (let ((failure nil)
           (byte-compile-warnings t)
           (original-load-path (copy-sequence load-path)))
       (push (expand-file-name \".\") original-load-path)
       (dolist (file command-line-args-left)
         (setq file (expand-file-name file))
         (let ((dir (file-name-directory file))
               (load-path (copy-sequence original-load-path))
               (tmpfile (make-temp-file \"check-byte-compile-\" nil \".el\")))
           (unwind-protect
               (progn
                 (push dir load-path)

                 (copy-file file tmpfile t)
                 (let ((default-directory dir))
                   (if (byte-compile-file tmpfile)
                       (message \"[ELISP BYTE-COMPILE] Success: %s\" file)
                     (setq failure t)
                     (message \"[ELISP BYTE-COMPILE] Failure: %s\" file))))
             (when (file-exists-p tmpfile)
               (delete-file tmpfile))
             (let ((dest (funcall (if (bound-and-true-p
                                       byte-compile-dest-file-function)
                                      byte-compile-dest-file-function
                                    #'byte-compile-dest-file)
                                  tmpfile)))
               (when (and dest
                          (file-exists-p dest))
                 (delete-file dest))))))
       (when failure
         (kill-emacs 1))))" \
  "$@"
