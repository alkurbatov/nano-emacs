# GNU Emacs / N Λ N O (developers edition)

**GNU Emacs / N Λ N O (developers edition)** is a set of configuration files
for GNU Emacs based on [nano-emacs](https://github.com/rougier/nano-emacs)
such as to provide a nice and consistent look and feel for developers.
The dark theme is based on [Nord colors](https://www.nordtheme.com/).

The philosophy of `nano-emacs` is to stick as much as possible to
vanilla Emacs without introducing too much dependencies (or none if
possible) and to keep it modular enough. The idea is for users to copy
the part they are interested in such as to include them in their own
configuration.

![preview](./images/nano-emacs-dark.png)

## Supported programming languages and formats

- `Bash`
- `C++`
- `Elisp`
- `Golang`
- `JSON`
- `LaTeX`
- `Lua`
- `Makefile`
- `Python`
- `TOML`
- `YAML`

## Limitations

- Only dark theme is supported.
- The configuration is tested on `Mac OS` and `Linux`.

## Mandatory requirements

- [GNU Emacs](https://www.gnu.org/software/emacs/) >= 30 with enabled tree-sitter.
  In case of `Mac OS`, [emacs-plus](https://github.com/d12frosted/homebrew-emacs-plus)
  is strongly recommended, e.g.:

  ```bash
  brew tap d12frosted/emacs-plus
  brew install emacs-plus@30 \
    --with-imagemagick \
    --with-savchenkovaleriy-big-sur-icon \
    --with-xwidgets \
    --with-poll
  ```

- Additional fonts

  ```bash
  brew install \
    homebrew/cask/font-roboto \
    homebrew/cask/font-roboto-slab \
    homebrew/cask/font-jetbrains-mono \
    homebrew/cask/font-jetbrains-mono-nerd-font
  ```

- `coreutls` (`Mac OS` only) for better listing in `Dired`

  ```bash
  brew install coreutls
  ```

- The `fd` utility (to search files with `consult`):

  ```bash
  brew install fd
  ```

### Optional requirements

- [Clangd](https://clangd.llvm.org/) for `LSP` in `C++`.
- [Golangci-lint](https://golangci-lint.run/) for linting in `Golang`.
- [Gopls](https://github.com/golang/tools/tree/master/gopls) for `LSP` in `Golang`.
- [Hunspell](https://github.com/hunspell/hunspell) for spellchecking.
- [Jsonlint](https://github.com/zaach/jsonlint) - to lint `JSON` files.
- [Markdownlint](https://github.com/DavidAnson/markdownlint-cli2) for markdown linting.
- [Pandoc](https://pandoc.org/) for markdown preview.
- [Poetry](https://python-poetry.org/) for virtual environment management in `Python`
- [Pyright](https://github.com/microsoft/pyright) for `LSP` in `Python`.
- `LaTeX` for formulas support in org mode, e.g.:

  ```bash
  brew install --cask mactex
  ```

- [LuaCheck](https://github.com/lunarmodules/luacheck) to link `Lua` files.

#### Recommended `Pyright` setup

To provide automatic activation of virtual environments create
`pyrightconfig.json` file in the root of your project with the following content:

``` json
{
  "venvPath": "<path-to-the-folder-containing-venv",
  "venv": "<name of the venv folder, e.g. .venv"
}
```

#### Common clipboard in Linux

To use common clipboard on Linux running under Wayland one needs to install
`xsel`:

```bash
sudo apt-get install xsel
```

### Installation

To install the project execute the following command:

```bash
make install
```
