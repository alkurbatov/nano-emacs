;;; init.el --- Main initialization routine -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2025 - N Λ N O developers

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
      '(auctex                   ; Integrated environment for *TeX*
        avy                      ; Jump to things in Emacs tree-style
        consult                  ; Consulting completing-read
        consult-eglot            ; Jump to workspace symbols with eglot and consult
        company                  ; Modular auto-completion framework
        diff-hl                  ; Package for highlighting uncommitted changes
        dired-quick-sort         ; Persistent quick sorting of dired buffers in various ways
        dock                     ; Integrate desktop environment's taskbar/dock with Emacs
        drag-stuff               ; Drag stuff around in Emacs. Like in Eclipse
        engine-mode              ; Minor mode for defining and querying search engines through Emacs
        evil-nerd-commenter      ; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
        exec-path-from-shell     ; Make Emacs use the $PATH set up by the user's shell
        f                        ; Modern API for working with files and directories
        flatbuffers-mode         ; Emacs mode for flatbuffers featuring syntax highlighting
        flyspell-correct-popup   ; Correcting words with flyspell via popup interface
        flyspell-popup           ; Correcting words with flyspell in popup menus
        format-all               ; Auto-format source code in many languages with one command
        git-link                 ; Emacs package to get the GitHub/Bitbucket/GitLab/... URL for a buffer location
        git-modes                ; Emacs major modes for Git configuration files
        go-impl                  ; impl for Emacs
        gotest                   ; Emacs mode to go unit test command line tool
        hl-todo                  ; Highlight TODO keywords
        jinx                     ; Enchanted Spell Checker
        ligature                 ; Display typographical ligatures in Emacs
        marginalia               ; Enrich existing commands with completion annotations
        markdown-mode            ; Major mode for Markdown-formatted text
        markdown-toc             ; Generate a TOC in markdown file
        orderless                ; Completion style for matching regexps in any order
        org-auto-tangle          ; Tangle org file when it is saved
        pinentry                 ; GnuPG Pinentry server implementation
        poetry                   ; Python dependency management and packaging in Emacs
        raku-mode                ; Emacs major mode for Raku
        realgud                  ; The Grand "Cathedral" Debugger rewrite
        rfc-mode                 ; An Emacs major mode to read and browse RFC documents
        russian-holidays         ; Russian holidays for GNU/Emacs calendar
        s                        ; The long lost Emacs string manipulation library
        ssh-config-mode          ; Emacs mode for editing ssh config files
        vertico                  ; VERTical Interactive Completion
        vterm                    ; Emacs libvterm integration
        ws-butler                ; Unobtrusively trim extraneous white-space *ONLY* in lines edited
        yasnippet))              ; A template system for Emacs

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29
(straight-use-package
 '(treesit-auto :type git :host github :repo "renzmann/treesit-auto"
                :fork (:host github :repo "alkurbatov/treesit-auto")))

;; Major mode for editing pip requirements files
(straight-use-package
 '(pip-requirements :type git :host github :repo "Wilfred/pip-requirements.el"
                    :fork (:host github :repo "alkurbatov/pip-requirements.el")))

;; GNU Emacs / N Λ N O Modeline
(straight-use-package
 '(nano-modeline :type git :host github :repo "rougier/nano-modeline" :branch "rewrite"))

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; Collection of flymake checkers
(straight-use-package
 '(flymake-collection :type git :host github :repo "mohkale/flymake-collection"
                      :fork (:host github :repo "alkurbatov/flymake-collection")))

;; Flymake diagnostics on cursor hover
(straight-use-package
 '(flymake-popon
   :type git
   :repo "https://codeberg.org/akib/emacs-flymake-popon.git"))

;; An Emacs minor mode for highlighting matches to the selection
(straight-use-package
 '(selection-highlight-mode :type git :host github :repo "balloneij/selection-highlight-mode"))

;; Tree sitter support for Protobuf
(straight-use-package
 '(protobuf-ts-mode :type git :host github :repo "emacsattic/protobuf-ts-mode"))

;; Tree sitter support for Makefiles
(straight-use-package
 '(makefile-ts-mode :type git :host github :repo "nverno/makefile-ts-mode"))

;; Emacs major mode for readline inputrc configs
(straight-use-package
 '(inputrc-mode :type git :host github :repo "nverno/inputrc-mode"))

;; Emacs mode for Bazel
(straight-use-package
 '(emacs-bazel-mode :type git :host github :repo "bazelbuild/emacs-bazel-mode"))

;; Fast, configurable indentation guide-bars for Emacs
(straight-use-package
 '(indent-bars :type git :host github :repo "jdtsmith/indent-bars"))

;; Run subtests and test with TreeSitter and gotest.el
(straight-use-package
 '(gotest-ts :type git :host github :repo "chmouel/gotest-ts.el"))

(straight-use-package
 '(nord-theme :type git :host github :repo "alkurbatov/nord-theme"))

;; Load settings
(require 'nano)

;;; init.el ends here
