;;; nano-lint.el --- Linters configuration -*- lexical-binding: t; -*-

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

(defun nano-enable-flymake ()
  "Enable Flymake."
  ;; Don't use left fringe, it is occupied by diff-hl
  (setq flymake-fringe-indicator-position nil)

  (flymake-mode)
  (flymake-popon-mode))

(defun nano-consult-flymake-project ()
  "Show Flymake errors from all buffers of the currently opened project."
  (interactive)
  (consult-flymake (project-current nil)))

(with-eval-after-load 'flymake
  ;; Provide some Flycheck-like bindings in Flymake mode to ease transition
  (bind-keys :map flymake-mode-map
             ("C-c ! b" . flymake-show-buffer-diagnostics)
             ("C-c ! P" . flymake-show-project-diagnostics)
             ("C-c ! L" . nano-consult-flymake-project)
             ("C-c ! l" . consult-flymake)
             ("C-c ! n" . flymake-goto-next-error)
             ("C-c ! p" . flymake-goto-prev-error)
             ("C-c ! c" . flymake-start)
             ("C-c ! v" . flymake-running-backends)))

(add-hook 'prog-mode-hook #'nano-enable-flymake)

(provide 'nano-lint)
;;; nano-lint.el ends here
