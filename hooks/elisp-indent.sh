#!/usr/bin/env sh
#
# Author: James Cherti
# URL: https://github.com/jamescherti/pre-commit-elisp
#
# Description:
# ------------
# Indent Elisp files according to Emacs Lisp style conventions.
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
  "(dolist (file command-line-args-left)
     (message \"[ELISP INDENT] %s\" file)
     (with-temp-buffer
       (put 'cl-letf 'lisp-indent-function 1)
       (put 'lightemacs-use-package 'lisp-indent-function 0)
       (put 'lightemacs-define-keybindings 'lisp-indent-function 1)
       (put 'lightemacs-verbose-message 'lisp-indent-function 0)
       (put 'lightemacs-save-window-hscroll 'lisp-indent-function 0)
       (put 'lightemacs-define-mode-add-hook-to 'lisp-indent-function 0)
       (put 'lightemacs-save-window-hscroll 'lisp-indent-function 0)
       (put 'lightemacs-save-window-start 'lisp-indent-function 0)

       ;; Modify settings
       (setq-local lexical-binding t)
       (emacs-lisp-mode)
       ;; (setq-local standard-indent 2)
       (setq-local indent-tabs-mode nil)
       (setq-local lisp-body-indent 2)
       ;; (setq lisp-indent-function #'lisp-indent-function)
       ;; (setq-local lisp-indent-offset nil)
       (setq-local tab-width 2)
       (insert-file-contents file)

       ;; Remove tabs
       (save-excursion
         (goto-char (point-min))
         (while (not (eobp))
           (let ((line-beg (line-beginning-position)))
             (goto-char line-beg)
             (skip-chars-forward \" \t\")
             (let ((indent-end (point)))
               (when (> indent-end line-beg)
                 (untabify line-beg indent-end))))
           (forward-line 1)))

       ;; Reindent
       (let ((beg (point-min))
             (end (point-max)))
         (save-restriction
           (narrow-to-region beg end)
           (if (save-excursion (goto-char beg)
                               (= end (line-beginning-position 2)))
               (indent-according-to-mode)
             (goto-char beg)
             (indent-region beg end))))

       (write-region (point-min) (point-max) file)))" \
  "$@"
