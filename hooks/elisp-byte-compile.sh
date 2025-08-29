#!/bin/bash
#
# Author: James Cherti
# URL: https://github.com/jamescherti/pre-commit-elisp
#
# Description:
# ------------
# Byte-compile Elisp files to detect compilation errors.
#
# License:
# --------
# Copyright (C) 2012-2025 James Cherti
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

emacs --batch --eval \
  "(with-temp-buffer
     (setq-local lexical-binding t)
     (setq byte-compile-warnings t)
     (let ((failure nil)
           (original-load-path (copy-sequence load-path)))
       (push (expand-file-name \".\") original-load-path)
       (dolist (file command-line-args-left)
         (setq file (expand-file-name file))
         (let ((dir (file-name-directory file))
               (load-path (copy-sequence original-load-path)))
           (push dir load-path)

           (let ((default-directory dir))
             (if (byte-compile-file file)
                 (message \"[ELISP BYTE-COMPILE] Success: %s\" file)
               (setq failure t)
               (message \"[ELISP BYTE-COMPILE] Failure: %s\" file)))))
       (when failure
         (kill-emacs 1))))" \
  "$@"
