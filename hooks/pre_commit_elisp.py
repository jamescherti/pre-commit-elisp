#!/usr/bin/env python
"""The pre-commit-elisp library."""

import os
import subprocess
import sys

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))

# Path to pre-commit-elisp.el (same directory as this script)
PRE_COMMIT_ELISP_LIB = os.path.join(SCRIPT_DIR, "pre-commit-elisp.el")

ENV = os.environ.copy()
ENV["PRE_COMMIT_ELISP_LIB"] = PRE_COMMIT_ELISP_LIB


def run_elisp(elisp_code: str):
    """Run the elisp_code Elisp code."""

    pre_commit_elisp_lib = ENV["PRE_COMMIT_ELISP_LIB"]
    if not os.path.exists(pre_commit_elisp_lib):
        print(f"Error: PRE_COMMIT_ELISP_LIB environment variable is not set "
              "or points to a non-existent file.", file=sys.stderr)
        sys.exit(1)

    # Build Emacs command
    cmd = ["emacs", "--batch", "--eval", elisp_code] + sys.argv[1:]

    # Run Emacs
    returncode = 0
    try:
        result = subprocess.run(cmd, env=ENV, check=True)
        returncode = result.returncode
    except subprocess.CalledProcessError as err:
        returncode = err.returncode
        print(f"Error: {err}", file=sys.stderr)

    return returncode
