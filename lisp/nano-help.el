;;; nano-help.el --- Help and embedded documentation improvements -*- lexical-binding: t; -*-

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
(require 'engine-mode)
(require 'which-key)

;; Enable which key mode to show available key bindings
(which-key-mode)

;; Open which-key hints faster (default is 1.0)
(setq which-key-idle-delay 0.2)

;; Binded to 'C-x /' by default
(engine-mode t)

(defengine cppreference
           "https://duckduckgo.com/?sites=cppreference.com&q=%s&ia=web"
           :keybinding "c")

(defengine pypi
           "https://pypi.org/search/?q=%s"
           :keybinding "y")

(defengine github
           "https://github.com/search?q=%s&type=code"
           :keybinding "h")

(defengine gopkg
           "https://pkg.go.dev/search?q=%s"
           :keybinding "g")

;; Short version of man pages.
(use-package tldr)

;; A better help buffer.
(use-package helpful
  :bind
  (("C-h f" . helpful-callable)   ; Look up callable.
   ("C-h v" . helpful-variable)   ; Look up variable.
   ("C-h k" . helpful-key)        ; Look up key.
   ("C-c C-d" . helpful-at-point) ; Look up the current symbol at point.
   ("C-h F" . helpful-function)   ; Look up *F*unctions (excludes macros).
   ("C-h C" . helpful-command)))  ; Look up *C*ommands.

(provide 'nano-help)
;;; nano-help.el ends here
