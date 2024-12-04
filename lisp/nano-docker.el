;;; nano-docker.el --- Docker programming environment -*- lexical-binding: t; -*-

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

;; Configures programming environment for Docker.

;;; Code:

;; Enable tree-sitter integration.
(add-to-list 'treesit-auto-langs 'dockerfile)

;; Enable syntax highlighting for Docker-related configuration files
(add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . gitignore-mode))
(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))

(provide 'nano-docker)
;;; nano-docker.el ends here
