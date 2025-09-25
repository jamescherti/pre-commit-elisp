#!/usr/bin/env python
"""Test the class Util()."""

import os
import subprocess
from pathlib import Path

import pytest

TEST_FILES = Path("./tests/files")
CHECK_PARENS_ERROR = TEST_FILES / "check_parens_error.el"
GOOD = TEST_FILES / "good.el"

ELISP_BYTE_COMPILE = Path("hooks/elisp-byte-compile.py")
ELISP_CHECK_BYTE_COMPILE = Path("hooks/elisp-check-byte-compile.py")
ELISP_CHECK_PARENS = Path("hooks/elisp-check-parens.py")
ELISP_INDENT = Path("hooks/elisp-indent.py")


def run_hook(script: Path, *files: Path):
    """Run an Elisp hook script on one or more files."""
    cmd = ["python", str(script)] + [str(f) for f in files]
    result = subprocess.run(cmd, capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr)
    return result


def test_check_byte_compile_good():
    """Byte-compile a good Elisp file."""
    result = run_hook(ELISP_CHECK_BYTE_COMPILE, GOOD)
    assert result.returncode == 0


def test_check_byte_compile_error():
    """Byte-compile a file that should fail (if any)."""
    result = run_hook(ELISP_CHECK_BYTE_COMPILE, CHECK_PARENS_ERROR)
    assert result.returncode != 0


def test_byte_compile_good():
    """Byte-compile a good Elisp file."""
    elc_file = Path(str(GOOD) + "c")
    if elc_file.exists():
        os.unlink(elc_file)

    result = run_hook(ELISP_BYTE_COMPILE, GOOD)
    assert result.returncode == 0
    assert elc_file.exists()  # elc file


def test_byte_compile_error():
    """Byte-compile a file that should fail (if any)."""
    result = run_hook(ELISP_BYTE_COMPILE, CHECK_PARENS_ERROR)
    assert result.returncode != 0


def test_check_parens_good():
    """Check parentheses on a correct file."""
    result = run_hook(ELISP_CHECK_PARENS, GOOD)
    assert result.returncode == 0


def test_check_parens_check_parens_error():
    """Check parentheses on a correct file."""
    result = run_hook(ELISP_CHECK_PARENS, CHECK_PARENS_ERROR)
    assert result.returncode != 0


def test_indent_good():
    """Run indent hook on a good file."""
    # TODO: Check the content of the file after indenting it
    result = run_hook(ELISP_INDENT, GOOD)
    assert result.returncode == 0
