;;; nano-spellcheck.el --- Setup spell checking -*- lexical-binding: t; -*-

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
;; Enable spellchecking with jinx.

;;; Code:

(use-package jinx
  :config
  (setopt jinx-languages "en_US ru_RU")

  :hook
  (emacs-startup . global-jinx-mode)

  :bind
  (("M-4" . jinx-correct)))

(provide 'nano-spellcheck)
;;; nano-spellcheck.el ends here
