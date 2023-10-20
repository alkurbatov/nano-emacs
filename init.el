;;; init.el --- Main initialization routine

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023 - N Λ N O developers

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
             "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e")

;; Setup dependencies
(require 'nano-package)

(setq package-list
      '(ag                      ; Integration of ag search utility
        avy                     ; Jump to things in Emacs tree-style
        bind-key                ; A simple way to manage personal keybindings
        cape                    ; Completion At Point Extensions
        company                 ; Modular auto-completion framework
        consult                 ; Consulting completing-read
        consult-recoll          ; Consult interface for recoll query
        corfu                   ; COmpletion Overlay Region Function
        counsel                 ; Improves ivy integration
        deft                    ; Quickly browse, filter, and edit plain text notes
        diff-hl                 ; Package for highlighting uncommitted changes
        elfeed                  ; An Emacs web feeds client
        elfeed-org              ; Configure the Elfeed RSS reader with an Orgmode file
        editorconfig            ; EditorConfig plugin for Emacs
        engine-mode             ; Minor mode for defining and querying search engines through Emacs
        ethan-wspace            ; A non-intrusive emacs customization for people who are OCD about whitespace
        evil-nerd-commenter     ; Comment/uncomment lines efficiently. Like Nerd Commenter in Vim
        exec-path-from-shell    ; Make Emacs use the $PATH set up by the user's shell
        f                       ; Modern API for working with files and directories
        flycheck                ; Modern on-the-fly syntax checking extension
        flycheck-eglot          ; Flycheck support for eglot
        flycheck-inline         ; Display Flycheck errors inline
        flyspell-correct-popup  ; Correcting words with flyspell via popup interface
        flyspell-popup          ; Correcting words with flyspell in popup menus
        format-all              ; Auto-format source code in many languages with one command
        gcmh                    ; The Garbage Collector Magic Hack
        git-modes               ; Emacs major modes for Git configuration files
        google-c-style          ; Google's C/C++ style for c-mode
        golden-ratio            ; Automatic resizing of Emacs windows to the golden ratio
        guess-language          ; Robust automatic language detection
        helpful                 ; A better help buffer
        highlight-indent-guides ; Emacs minor mode to highlight indentation
        hl-todo                 ; Highlight TODO keywords
        htmlize                 ; Convert buffer text and decorations to HTML
        imenu-list              ; Show imenu entries in a separate buffer
        ligature                ; Display typographical ligatures in Emacs
        magit                   ; A Git porcelain inside Emacs.
        marginalia              ; Enrich existing commands with completion annotations
        markdown-mode           ; Major mode for Markdown-formatted text
        markdown-toc            ; Generate a TOC in markdown file
        multi-term              ; Managing multiple terminal buffers in Emacs.
        orderless               ; Completion style for matching regexps in any order
        org-auto-tangle         ; Tangle org file when it is saved
        org-superstar           ; Make org-mode stars a little more super
        pinentry                ; GnuPG Pinentry server implementation
        poetry                  ; Python dependency management and packaging in Emacs
        protobuf-mode           ; Major mode for editing protocol buffers
        realgud                 ; The Grand "Cathedral" Debugger rewrite
        ssh-config-mode         ; Emacs mode for editing ssh config files
        smartparens             ; Minor mode for Emacs that deals with parens pairs and tries to be smart about it
        smex                    ; A smart M-x enhancement for Emacs
        treesit-auto            ; Automatic installation, usage, and fallback for tree-sitter major modes in Emacs 29
        vertico                 ; VERTical Interactive Completion
        yaml-mode               ; YAML mode
        yasnippet               ; A template system for Emacs
        which-key))             ; Display available keybindings in popup

;; Install packages that are not yet installed
(dolist (package package-list)
  (straight-use-package package))

;; Major mode for editing pip requirements files
(straight-use-package
 '(pip-requirements :type git :host github :repo "Wilfred/pip-requirements.el"))

;; GNU Emacs / N Λ N O Theme
(straight-use-package
 '(nano-theme :type git :host github :repo "rougier/nano-theme"))

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

;; SVG tags, progress bars & icons
(straight-use-package
 '(svg-lib :type git :host github :repo "rougier/svg-lib"))

;; Replace keywords with SVG tags
(straight-use-package
 '(svg-tag-mode :type git :host github :repo "rougier/svg-tag-mode"))

;; Dashboard for mu4e
(straight-use-package
 '(mu4e-dashboard :type git :host github :repo "rougier/mu4e-dashboard"))

;; Folding mode for mu4e
(straight-use-package
 '(mu4e-folding :type git :host github :repo "rougier/mu4e-folding"))

;; Relative date formatting
(straight-use-package
 '(relative-date :type git :host github :repo "rougier/relative-date"))

;; org imenu
(straight-use-package
 '(org-imenu :type git :host github :repo "rougier/org-imenu"))

;; pdf-drop-mode
(straight-use-package
 '(pdf-drop-mode :type git :host github :repo "rougier/pdf-drop-mode"))

;; Flycheck checker for golangci-lint
(straight-use-package
 '(flycheck-golangci-lint :type git :host github :repo "weijiangan/flycheck-golangci-lint"
            :fork (:host github
                   :repo "alkurbatov/flycheck-golangci-lint")))

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

;; Load settings
(require 'nano)

;;; init.el ends here
