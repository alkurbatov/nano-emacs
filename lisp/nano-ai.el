;;; nano-ai.el --- AI tool integration -*- lexical-binding: t; -*-

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
(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")

  :commands (claude-code-ide)

  :config
  (claude-code-ide-emacs-tools-setup)

  :bind
  ("C-c C-'" . claude-code-ide-menu))

(provide 'nano-ai)
;;; nano-ai.el ends here
