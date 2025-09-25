# pre-commit-elisp - Emacs Lisp (Elisp) Git pre-commit hooks

The [pre-commit-elisp](https://github.com/jamescherti/pre-commit-elisp) repository offers pre-commit hooks for **Emacs Lisp (Elisp)** projects. These hooks enforce code quality and consistency by performing automated checks on `.el` files prior to committing changes:

* **`elisp-check-parens`**: Validates that all parentheses in `.el` files are correctly balanced.
* **`elisp-check-byte-compile`**: Byte-compile Elisp files to detect compilation errors.
* **`elisp-indent`**: Indent Elisp files according to Emacs Lisp style conventions.

These pre-commit hooks enforce syntactic correctness, successful byte-compilation, and consistent code formatting, ensuring a high standard of code quality and maintainability throughout the repository.

If this enhances your workflow, please show your support by **⭐ starring pre-commit-elisp on GitHub** to help more Emacs users discover its benefits.

## Installation

1. Install [pre-commit](https://pre-commit.com/).

2. Add this repository as a local hook in your `.pre-commit-config.yaml`:

```yaml
---

repos:
  - repo: https://github.com/jamescherti/pre-commit-elisp
    rev: v1.0.5
    hooks:
      # Validate that all parentheses in .el files are correctly balanced
      - id: elisp-check-parens

      # Optional: Byte-compile .el files to identify compilation errors early
      # - id: elisp-check-byte-compile

      # Optional: Indent Elisp files according to Emacs Lisp style conventions
      # - id: elisp-indent
```

3. Install the hooks in your project:

```bash
pre-commit install
```

4. Run hooks manually on all files (optional):

```bash
pre-commit run --all-files
```

## Customizations

#### Customizing load-path

Scripts such as `elisp-check-byte-compile` and `elisp-byte-compile` support customizing the `load-path` variable using a `.dir-locals.el` variable `pre-commit-elisp-load-path`. This variable allows specifying the directories that should be included in the `load-path` without modifying the scripts themselves, ensuring that dependencies and libraries located in the project or its subdirectories are correctly available for byte-compilation.

Customizing the `load-path` allows the byte-compilation scripts, such as `elisp-check-byte-compile`, to **find and load project-specific Emacs Lisp files** during compilation.

Here is an example of a `.dir-locals.el` file to place at the root of the Git repository:

```elisp
((nil . ((pre-commit-elisp-load-path . ("." "lib/" "utils")))))
```

The `pre-commit-elisp-load-path` list is a **list of directories** relative to the Git repository root or project directory.

Each entry in the list determines how it is added to `load-path`:

1. **Directory ends with a slash (`/`)**: Recursively adds the directory and all its subdirectories to `load-path`. Example: `"lib/"` adds `lib/` and all its subdirectories.

2. **Directory does not end with a slash**: The directory is added non-recursively. Example: `"utils"` adds only the `utils` directory, not its subdirectories.

## License

The pre-commit-elisp hooks have been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2025 [James Cherti](https://www.jamescherti.com)

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with this program.

## Links

- [pre-commit-elisp @GitHub](https://github.com/jamescherti/pre-commit-elisp)

Other Emacs packages by the same author:
- [minimal-emacs.d](https://github.com/jamescherti/minimal-emacs.d): This repository hosts a minimal Emacs configuration designed to serve as a foundation for your vanilla Emacs setup and provide a solid base for an enhanced Emacs experience.
- [compile-angel.el](https://github.com/jamescherti/compile-angel.el): **Speed up Emacs!** This package guarantees that all .el files are both byte-compiled and native-compiled, which significantly speeds up Emacs.
- [outline-indent.el](https://github.com/jamescherti/outline-indent.el): An Emacs package that provides a minor mode that enables code folding and outlining based on indentation levels for various indentation-based text files, such as YAML, Python, and other indented text files.
- [vim-tab-bar.el](https://github.com/jamescherti/vim-tab-bar.el): Make the Emacs tab-bar Look Like Vim’s Tab Bar.
- [elispcomp](https://github.com/jamescherti/elispcomp): A command line tool that allows compiling Elisp code directly from the terminal or from a shell script. It facilitates the generation of optimized .elc (byte-compiled) and .eln (native-compiled) files.
- [tomorrow-night-deepblue-theme.el](https://github.com/jamescherti/tomorrow-night-deepblue-theme.el): The Tomorrow Night Deepblue Emacs theme is a beautiful deep blue variant of the Tomorrow Night theme, which is renowned for its elegant color palette that is pleasing to the eyes. It features a deep blue background color that creates a calming atmosphere. The theme is also a great choice for those who miss the blue themes that were trendy a few years ago.
- [Ultyas](https://github.com/jamescherti/ultyas/): A command-line tool designed to simplify the process of converting code snippets from UltiSnips to YASnippet format.
- [dir-config.el](https://github.com/jamescherti/dir-config.el): Automatically find and evaluate .dir-config.el Elisp files to configure directory-specific settings.
- [flymake-bashate.el](https://github.com/jamescherti/flymake-bashate.el): A package that provides a Flymake backend for the bashate Bash script style checker.
- [flymake-ansible-lint.el](https://github.com/jamescherti/flymake-ansible-lint.el): An Emacs package that offers a Flymake backend for ansible-lint.
- [inhibit-mouse.el](https://github.com/jamescherti/inhibit-mouse.el): A package that disables mouse input in Emacs, offering a simpler and faster alternative to the disable-mouse package.
- [quick-sdcv.el](https://github.com/jamescherti/quick-sdcv.el): This package enables Emacs to function as an offline dictionary by using the sdcv command-line tool directly within Emacs.
- [enhanced-evil-paredit.el](https://github.com/jamescherti/enhanced-evil-paredit.el): An Emacs package that prevents parenthesis imbalance when using *evil-mode* with *paredit*. It intercepts *evil-mode* commands such as delete, change, and paste, blocking their execution if they would break the parenthetical structure.
- [stripspace.el](https://github.com/jamescherti/stripspace.el): Ensure Emacs Automatically removes trailing whitespace before saving a buffer, with an option to preserve the cursor column.
- [persist-text-scale.el](https://github.com/jamescherti/persist-text-scale.el): Ensure that all adjustments made with text-scale-increase and text-scale-decrease are persisted and restored across sessions.
- [pathaction.el](https://github.com/jamescherti/pathaction.el): Execute the pathaction command-line tool from Emacs. The pathaction command-line tool enables the execution of specific commands on targeted files or directories. Its key advantage lies in its flexibility, allowing users to handle various types of files simply by passing the file or directory as an argument to the pathaction tool. The tool uses a .pathaction.yaml rule-set file to determine which command to execute. Additionally, Jinja2 templating can be employed in the rule-set file to further customize the commands.
