#!/usr/bin/env python
"""Test the class Util()."""

import subprocess
import tempfile
from pathlib import Path

import pytest

TEST_FILES_DIRECTORY: Path = Path("./tests/files")
FAILURE_FILE: Path = TEST_FILES_DIRECTORY / "check_parens_error.el"
SUCCESS_FILE: Path = TEST_FILES_DIRECTORY / "good.el"
SUCCESS_FILE_INDENTED: Path = TEST_FILES_DIRECTORY / "good_indented.el"

SCRIPT_BYTE_COMPILE: Path = Path("hooks/elisp-byte-compile.py")
SCRIPT_CHECK_BYTE_COMPILE: Path = Path("hooks/elisp-check-byte-compile.py")
SCRIPT_CHECK_NATIVE_COMPILE: Path = Path("hooks/elisp-check-native-compile.py")
SCRIPT_CHECK_PARENS: Path = Path("hooks/elisp-check-parens.py")
SCRIPT_INDENT: Path = Path("hooks/elisp-indent.py")


def run_hook(script: Path, *files: Path) -> subprocess.CompletedProcess[str]:
    """Run an Elisp hook script on one or more files.

    :param script: The path to the script.
    :type script: Path
    :param files: The paths to the files to process.
    :type files: Path
    :return: The completed process result.
    :rtype: subprocess.CompletedProcess[str]
    """
    cmd: list[str] = ["python", str(script)] + [str(f) for f in files]
    result: subprocess.CompletedProcess[str] = subprocess.run(
        cmd, capture_output=True, text=True)
    print(result.stdout)
    print(result.stderr)
    return result

# -----------------------------------------------------------------------------
# CHECK BYTE COMPILE
# -----------------------------------------------------------------------------


def test_check_byte_compile_success() -> None:
    """Byte-compile a good Elisp file.

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_BYTE_COMPILE, SUCCESS_FILE)
    assert result.returncode == 0


def test_check_byte_compile_failure() -> None:
    """Byte-compile a file that should fail (if any).

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_BYTE_COMPILE, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# BYTE COMPILE
# -----------------------------------------------------------------------------


def test_byte_compile_success() -> None:
    """Byte-compile a good Elisp file.

    :return: None.
    :rtype: None
    """
    elc_file: Path = Path(str(SUCCESS_FILE) + "c")
    elc_file.unlink(missing_ok=True)

    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_BYTE_COMPILE, SUCCESS_FILE)
    assert result.returncode == 0
    assert elc_file.exists()


def test_byte_compile_failure() -> None:
    """Byte-compile a file that should fail (if any).

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_BYTE_COMPILE, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# CHECK NATIVE COMPILE
# -----------------------------------------------------------------------------


def test_check_native_compile_success() -> None:
    """Native-compile a good Elisp file.

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_NATIVE_COMPILE, SUCCESS_FILE)
    assert result.returncode == 0


def test_check_native_compile_failure() -> None:
    """Native-compile a file that should fail.

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_NATIVE_COMPILE, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# CHECK PARENS
# -----------------------------------------------------------------------------


def test_check_parens_success() -> None:
    """Check parentheses on a correct file.

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_PARENS, SUCCESS_FILE)
    assert result.returncode == 0


def test_check_parens_failure() -> None:
    """Check parentheses on a correct file.

    :return: None.
    :rtype: None
    """
    result: subprocess.CompletedProcess[str] = run_hook(
        SCRIPT_CHECK_PARENS, FAILURE_FILE)
    assert result.returncode != 0


# -----------------------------------------------------------------------------
# INDENT
# -----------------------------------------------------------------------------

def test_indent() -> None:
    """Run indent hook and verify indentation changed the content.

    :return: None.
    :rtype: None
    """
    original_content: str = SUCCESS_FILE.read_text(encoding="utf-8")

    with tempfile.NamedTemporaryFile(delete=False, suffix=".el") as tmp:
        tmp_path: Path = Path(tmp.name)
        tmp.write(original_content.encode("utf-8"))

    try:
        result: subprocess.CompletedProcess[str] = run_hook(
            SCRIPT_INDENT, tmp_path)
        assert result.returncode == 0

        expected_content: str = SUCCESS_FILE_INDENTED.read_text(
            encoding="utf-8")
        new_content: str = tmp_path.read_text(encoding="utf-8")

        assert expected_content == new_content
    finally:
        tmp_path.unlink(missing_ok=True)
