;;; init.el --- Main initialization routine -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2024 - N Λ N O developers

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main initialization code. Contains general settings and
;; installation of dependencies.

;;; Code:

;; Libraries load path
(add-to-list 'load-path
             (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path
             (expand-file-name "theme" user-emacs-directory))

;; Setup dependencies
(require 'nano-package)

(setq package-list
      '(ag                       ; Integration of ag search utility
        avy                      ; Jump to things in Emacs tree-style
        bind-key                 ; A simple way to manage personal keybindings
        consult                  ; Consulting completing-read
        company                  ; Modular auto-completion framework
        diff-hl                  ; Package for highlighting uncommitted changes
        dired-quick-sort         ; Persistent quick sorting of dired buffers in various ways
        elfeed                   ; An Emacs web feeds client
        elfeed-org               ; Configure the Elfeed RSS reader with an Orgmode file
        editorconfig             ; EditorConfig plugin for Emacs
        engine-mode              ; Minor mode for defining and querying search engines through Emacs
        ethan-wspace             ; A non-intrusive emacs customization for people who are OCD about whitespace
        evil-nerd-commenter      ; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
        exec-path-from-shell     ; Make Emacs use the $PATH set up by the user's shell
        f                        ; Modern API for working with files and directories
        flyspell-correct-popup   ; Correcting words with flyspell via popup interface
        flyspell-popup           ; Correcting words with flyspell in popup menus
        format-all               ; Auto-format source code in many languages with one command
        git-modes                ; Emacs major modes for Git configuration files
        helpful                  ; A better help buffer
        highlight-indent-guides  ; Emacs minor mode to highlight indentation
        hl-todo                  ; Highlight TODO keywords
        ligature                 ; Display typographical ligatures in Emacs
        magit                    ; A Git porcelain inside Emacs.
        marginalia               ; Enrich existing commands with completion annotations
        markdown-mode            ; Major mode for Markdown-formatted text
        markdown-toc             ; Generate a TOC in markdown file
        orderless                ; Completion style for matching regexps in any order
        org-auto-tangle          ; Tangle org file when it is saved
        org-superstar            ; Make org-mode stars a little more super
        pinentry                 ; GnuPG Pinentry server implementation
        poetry                   ; Python dependency management and packaging in Emacs
        realgud                  ; The Grand "Cathedral" Debugger rewrite
        rfc-mode                 ; An Emacs major mode to read and browse RFC documents.
        russian-holidays         ; Russian holidays for GNU/Emacs calendar
        ssh-config-mode          ; Emacs mode for editing ssh config files
        smartparens              ; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
        smex                     ; A smart M-x enhancement for Emacs
        treesit-auto             ; Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29
        vertico                  ; VERTical Interactive Completion
        yaml-mode                ; YAML mode
        yasnippet                ; A template system for Emacs
        which-key))              ; Display available keybindings in popup

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Major mode for editing pip requirements files
(straight-use-package
 '(pip-requirements :type git :host github :repo "Wilfred/pip-requirements.el"
                    :fork (:host github :repo "alkurbatov/pip-requirements.el")))

;; GNU Emacs / N Λ N O Modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline"))

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; Display org properties in the agenda buffer (modified version)
(straight-use-package
 '(org-agenda-property :type git :host github :repo "Malabarba/org-agenda-property"
                       :fork (:host github :repo "rougier/org-agenda-property")))

;; Relative date formatting
(straight-use-package
 '(relative-date :type git :host github :repo "rougier/relative-date"))

;; Collection of flymake checkers
(straight-use-package
 '(flymake-collection :type git :host github :repo "mohkale/flymake-collection"
                      :fork (:host github :repo "alkurbatov/flymake-collection")))

;; Flymake diagnostics on cursor hover
(straight-use-package
 '(flymake-popon
   :type git
   :repo "https://codeberg.org/akib/emacs-flymake-popon.git"))

;; Emulate A Terminal, in a region, in a buffer and in Eshell
(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; An Emacs minor mode for highlighting matches to the selection
(straight-use-package
 '(selection-highlight-mode :type git :host github :repo "balloneij/selection-highlight-mode"))

;; Tree sitter support for Protobuf
(straight-use-package
 '(protobuf-ts-mode :type git :host github :repo "ashlineldridge/protobuf-ts-mode"))

;; Tree sitter support for Makefiles
(straight-use-package
 '(makefile-ts-mode :type git :host github :repo "nverno/makefile-ts-mode"))

;; Emacs major mode for readline inputrc configs
(straight-use-package
 '(inputrc-mode :type git :host github :repo "nverno/inputrc-mode"))

(straight-use-package
 `(lspce :type git :host github :repo "zbelial/lspce"
         :files (:defaults ,(pcase system-type
                              ('gnu/linux "lspce-module.so")
                              ('darwin "lspce-module.dylib")))
         :pre-build ,(pcase system-type
                       ('gnu/linux '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.so" "./lspce-module.so")))
                       ('darwin '(("cargo" "build" "--release") ("cp" "./target/release/liblspce_module.dylib" "./lspce-module.dylib"))))))

;; Load settings
(require 'nano)

;;; init.el ends here
