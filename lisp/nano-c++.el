;;; nano-c++.el --- Python language programming environment

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

;;; Code:
(defun nano-setup-c++-with-eglot ()
  "Setup and enable Eglot for C++."

  (eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(c++-ts-mode
                 . ("clangd"
                    "-j=4"
                    "--log=error"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--background-index"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0"))))

(add-hook 'c++-ts-mode-hook #'nano-setup-c++-with-eglot)

;; Force C++ mode for all headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-ts-mode))

;; Enable syntax highlighting for C++-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.clang-format\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("CMakeLists.txt\\'" . cmake-ts-mode))

(provide 'nano-c++)
;;; nano-c++.el ends here
