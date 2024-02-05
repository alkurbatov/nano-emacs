;; nano.el --- Main entry point

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers

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

;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-debug" . (lambda (args))))

;; Theme
(require 'nano-theme-ex)
(load-theme 'nano-dark t)
(nano-mode)
(nano-theme-customize)

;; Settings
(require 'nano-settings)

;; Default layout
(require 'nano-layout)
(require 'nano-whitespace)

;; Nano default settings
(require 'nano-defaults)

;; Nano calendar tweaks
(require 'nano-calendar)

;; Nano session saving
(require 'nano-session)

;; Nano key bindings modification
(require 'nano-bindings)

;; Nano spellcheck configuration
(require 'nano-spellcheck)

;; Nano counsel configuration
(require 'nano-counsel)

;; Nano terminal configuration
(require 'nano-terminal)

;; Nano help and embedded documentation improvements
(require 'nano-help)

;; Welcome message
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O for devs edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Splash
(unless (member "-no-splash" command-line-args)
  (require 'nano-splash)
  (nano-splash))

;; Extended debugging
(when (member "-debug" command-line-args)
    (setq debug-on-error t))

;; Org mode
(require 'nano-org)

;; IDE features
(require 'nano-project)

;; Tools
(require 'nano-docker)
(require 'nano-git)
(require 'nano-mc)
(require 'nano-remote)
(require 'nano-rss)

;; Programming languages
(require 'nano-elisp)
(require 'nano-c++)
(require 'nano-go)
(require 'nano-make)
(require 'nano-markdown)
(require 'nano-python)
(require 'nano-sh)
(require 'nano-toml)
(require 'nano-yaml)

(provide 'nano)
;;; nano.el ends here
