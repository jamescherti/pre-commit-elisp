#!/usr/bin/env python
"""Test the class Util()."""

import os
import subprocess
import tempfile
from pathlib import Path

import pytest

TEST_FILES_DIRECTORY = Path("./tests/files")
FAILURE_FILE = TEST_FILES_DIRECTORY / "check_parens_error.el"
SUCCESS_FILE = TEST_FILES_DIRECTORY / "good.el"
SUCCESS_FILE_INDENTED = TEST_FILES_DIRECTORY / "good_indented.el"

SCRIPT_BYTE_COMPILE = Path("hooks/elisp-byte-compile.py")
SCRIPT_CHECK_BYTE_COMPILE = Path("hooks/elisp-check-byte-compile.py")
SCRIPT_CHECK_PARENS = Path("hooks/elisp-check-parens.py")
SCRIPT_INDENT = Path("hooks/elisp-indent.py")


def run_hook(script: Path, *files: Path):
    """Run an Elisp hook script on one or more files."""
    cmd = ["python", str(script)] + [str(f) for f in files]
    result = subprocess.run(cmd, capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr)
    return result

# -----------------------------------------------------------------------------
# CHECK BYTE COMPILE
# -----------------------------------------------------------------------------


def test_check_byte_compile_success():
    """Byte-compile a good Elisp file."""
    result = run_hook(SCRIPT_CHECK_BYTE_COMPILE, SUCCESS_FILE)
    assert result.returncode == 0


def test_check_byte_compile_failure():
    """Byte-compile a file that should fail (if any)."""
    result = run_hook(SCRIPT_CHECK_BYTE_COMPILE, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# BYTE COMPILE
# -----------------------------------------------------------------------------


def test_byte_compile_success():
    """Byte-compile a good Elisp file."""
    elc_file = Path(str(SUCCESS_FILE) + "c")
    if elc_file.exists():
        os.unlink(elc_file)

    result = run_hook(SCRIPT_BYTE_COMPILE, SUCCESS_FILE)
    assert result.returncode == 0
    assert elc_file.exists()


def test_byte_compile_failure():
    """Byte-compile a file that should fail (if any)."""
    result = run_hook(SCRIPT_BYTE_COMPILE, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# BYTE COMPILE
# -----------------------------------------------------------------------------


def test_check_parens_success():
    """Check parentheses on a correct file."""
    result = run_hook(SCRIPT_CHECK_PARENS, SUCCESS_FILE)
    assert result.returncode == 0


def test_check_parens_failure():
    """Check parentheses on a correct file."""
    result = run_hook(SCRIPT_CHECK_PARENS, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# INDENT
# -----------------------------------------------------------------------------


def test_indent():
    """Run indent hook and verify indentation changed the content."""
    original_content = SUCCESS_FILE.read_text(encoding="utf-8")

    with tempfile.NamedTemporaryFile(delete=False, suffix=".el") as tmp:
        tmp_path = Path(tmp.name)
        tmp.write(original_content.encode("utf-8"))

    try:
        result = run_hook(SCRIPT_INDENT, tmp_path)
        assert result.returncode == 0

        expected_content = SUCCESS_FILE_INDENTED.read_text(encoding="utf-8")
        new_content = tmp_path.read_text(encoding="utf-8")

        assert expected_content == new_content
    finally:
        tmp_path.unlink(missing_ok=True)
