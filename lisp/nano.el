;; nano.el --- Main entry point -*- lexical-binding: t; -*-

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

;;; Code:

(when (< emacs-major-version 30)
    (error (format "Nano Emacs for Devs only works with Emacs 30 and newer; you have version %d" emacs-major-version)))

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-debug" . (lambda (args))))

;; Settings
(require 'nano-settings)

;; Default layout
(require 'nano-font)
(require 'nano-appearance)
(require 'nano-layout)
(require 'nano-whitespace)

;; Nano default settings
(require 'nano-defaults)
(require 'nano-clipboard)
(require 'nano-scroll)

;; Nano calendar tweaks
(require 'nano-calendar)

;; Nano session saving
(require 'nano-session)

;; Nano key bindings modification
(require 'nano-bindings)

;; Nano spellcheck configuration
(require 'nano-spellcheck)

;; Nano terminal configuration
(require 'nano-terminal)

;; Nano help and embedded documentation improvements
(require 'nano-help)

;; Minibuffer configuration
(require 'nano-minibuffer-ex)

;; Welcome message
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O for devs edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Extended debugging
(when (member "-debug" command-line-args)
  (setq debug-on-error t))

;; Org mode
(require 'nano-org)

;; IDE features
(require 'nano-parens)
(require 'nano-lsp)
(require 'nano-project)

;; Tools
(require 'nano-ansible)
(require 'nano-compile)
(require 'nano-docker)
(require 'nano-lint)
(require 'nano-git)
(require 'nano-mc)
(require 'nano-remote)
(autoload 'nano-what-faces "nano-debug" "" t nil)

;; Programming languages
(require 'nano-c++)
(require 'nano-cmake)
(require 'nano-elisp)
(require 'nano-go)
(require 'nano-js)
(require 'nano-json)
(require 'nano-lua)
(require 'nano-make)
(require 'nano-markdown)
(require 'nano-protobuf)
(require 'nano-python)
(require 'nano-sh)
(require 'nano-toml)
(require 'nano-typescript)
(require 'nano-yaml)

(provide 'nano)
;;; nano.el ends here
