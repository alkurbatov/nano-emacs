;;; nano-minibuffer.el --- Minibuffer configuration -*- lexical-binding: t; -*-

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

;;; Code:
(require 'consult)
(require 'marginalia)
(require 'vertico)

(defun nano-vertico--match-directory (str)
  "Match directory delimiter in STR."
  (string-suffix-p "/" str))

(defun nano-vertico-sort-directories-first (files)
  "Sort directories before FILES."
  (setq files (vertico-sort-alpha files))
  (nconc (seq-filter #'nano-vertico--match-directory files)
         (seq-remove #'nano-vertico--match-directory files)))

;; Customize list of Consult sources for the consult-buffer command
(setq consult-buffer-sources '(consult--source-hidden-buffer
                               consult--source-modified-buffer
                               consult--source-buffer
                               consult--source-file-register
                               consult--source-project-buffer-hidden))

(setq consult-fd-args '("fd" "-i" "-H" "-c" "never"))

(setq vertico-resize nil        ; how to resize the Vertico minibuffer window
      vertico-count 20          ; maximal number of candidates to show
      vertico-count-format nil) ; no prefix with number of entries
(vertico-mode)

(setq vertico-multiform-categories
      '((file (vertico-sort-function . nano-vertico-sort-directories-first))))
(vertico-multiform-mode)

(setq-default marginalia--ellipsis "…"    ; nicer ellipsis
              marginalia-align 'right     ; right alignment
              marginalia-align-offset -1) ; one space on the right
(marginalia-mode)

;; Bind Consult commands
(bind-key "C-x f" #'consult-fd)
(bind-key "C-x b"   #'consult-buffer)
(bind-key "C-s"     #'consult-line)
(bind-key "C-c r"   #'consult-recent-file)
(bind-key "M-g g"   #'consult-goto-line)
(bind-key "M-g M-g" #'consult-goto-line)
(bind-key "M-g o"   #'consult-outline)

(provide 'nano-minibuffer)
;;; nano-minibuffer.el ends here
