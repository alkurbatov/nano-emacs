;;; nano-whitespace.el --- Configure whitespace mode

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

;; Inspired by
;; https://github.com/VernonGrant/discovering-emacs/blob/main/show-notes/4-using-whitespace-mode.md

;;; Code:

;; Define the whitespace style.
(setq-default whitespace-style
              '(face spaces empty tabs newline trailing space-mark tab-mark newline-mark))

;; Make these characters represent whitespace.
(setq-default whitespace-display-mappings
      '(
        ;; space -> ·
        (space-mark 32 [183])
        ;; new line -> $
        (newline-mark ?\n [36 ?\n])
        ;; carriage return (Windows) -> ¶
        (newline-mark ?\r [182])
        ;; tabs -> »
        (tab-mark ?\t [187 ?\t])))


;; Don't highlight long lines, there is not much we can do with them.
(setq-default whitespace-line-column nil)

(provide 'nano-whitespace)
;;; nano-whitespace.el ends here
