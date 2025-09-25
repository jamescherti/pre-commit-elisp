#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/pre-commit-elisp
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
"""Byte-compile Elisp files to detect compilation errors.

Alternative approach: To prevent .elc files from appearing in the repository,
use elisp-check-byte-compile instead of elisp-byte-compile. It performs
compilation checks entirely in temporary files."""

import sys

from pre_commit_elisp import run_elisp


def elisp_byte_compile():
    return run_elisp("""
    (with-temp-buffer
      (let ((lib (getenv "PRE_COMMIT_ELISP_LIB")))
        (if (and lib (file-exists-p lib))
            (load-file lib)
          (error
           "PRE_COMMIT_ELISP_LIB is not set or points to a non-existent file."
           )))

      (pre-commit-elisp-byte-compile "[ELISP BYTE-COMPILE] " nil))
    """)


if __name__ == "__main__":
    ERRNO = elisp_byte_compile()
    sys.exit(ERRNO)
