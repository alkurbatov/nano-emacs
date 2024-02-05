## GNU Emacs / N Λ N O (developers edition)

**GNU Emacs / N Λ N O (developers edition)** is a set of configuration files for GNU Emacs
based on [nano-emacs](https://github.com/rougier/nano-emacs) such as to provide a nice and consistent look and feel for developers.
The dark theme is based on [Nord colors](https://www.nordtheme.com/).

The philosophy of `nano-emacs` is to stick as much as possible to
vanilla Emacs without introducing too much dependencies (or none if
possible) and to keep it modular enough. The idea is for users to copy
the part they are interested in such as to include them in their own
configuration.

<div>
<img src="./images/nano-emacs-dark.png">
</div>

### Supported programming languages and formats
- `Bash`
- `C++`
- `Elisp`
- `Golang`
- `Makefile`
- `Python`
- `TOML`
- `YAML`

### Limitations
- Only dark theme is supported.
- The configuration is tested mostly on `Mac OS` and rarely on `Linux`.

### Mandatory requirements
- [GNU Emacs](https://www.gnu.org/software/emacs/) >= 29.1 with enabled tree-sitter and native compilation.
  In case of `Mac OS`, [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus) is strongly recommended, e.g.:
  ```bash
  brew tap d12frosted/emacs-plus
  brew install emacs-plus@29 --with-imagemagick --with-no-frame-refocus --with-savchenkovaleriy-big-sur-icon --with-xwidgets
  ```

- Additional fonts
  ```bash
  brew install font-fira-code font-roboto font-roboto-mono font-roboto-mono-nerd-font font-roboto-slab font-victor-mono homebrew/cask-fonts/font-jetbrains-mono
  ```

- `coreutls` (`Mac OS` only) for better listing in `Dired`
  ```bash
  brew install coreutls
  ```

### Optional requirements
- [Clangd](https://clangd.llvm.org/) for `LSP` in `C++`.
- [Golangci-lint](https://golangci-lint.run/) for better linting in `Golang`.
- [Gopls](https://github.com/golang/tools/tree/master/gopls) for `LSP` in `Golang`.
- [golangci-lint](https://golangci-lint.run/) for linting of `Golang` files.
- [Hunspell](https://github.com/hunspell/hunspell) for spellchecking.
- [Pandoc](https://pandoc.org/) for markdown preview
- [Poetry](https://python-poetry.org/) for virtual environment management in `Python`
- [Pyright](https://github.com/microsoft/pyright) for `LSP` in `Python`.

### Installation
To install the project execute the following command:
```bash
make install
```
