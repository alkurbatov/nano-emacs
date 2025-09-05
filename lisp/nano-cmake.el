;;; nano-cmake.el --- CMake programming environment -*- lexical-binding: t; -*-

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
(defun nano-setup-cmake ()
  "Setup cmake."

  ;; See https://docs.zephyrproject.org/latest/contribute/style/cmake.html
  (setq cmake-ts-mode-indent-offset 2))

(add-hook 'cmake-ts-mode-hook #'nano-setup-cmake)
(add-hook 'cmake-ts-mode-hook #'indent-bars-mode)

;; Enable tree-sitter integration.
(add-to-list 'treesit-auto-langs 'cmake)

(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt\\'" . cmake-ts-mode))

(provide 'nano-cmake)
;;; nano-cmake.el ends here
