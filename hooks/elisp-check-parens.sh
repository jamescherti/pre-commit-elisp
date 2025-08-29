#!/bin/bash
#!/bin/bash
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

emacs --batch --eval '(dolist (file command-line-args-left)
                        (message "[ELISP CHECK-PARENS] %s" file)
                        (with-temp-buffer
                          (emacs-lisp-mode)
                          (insert-file-contents file)
                          (check-parens)))' "$@"
