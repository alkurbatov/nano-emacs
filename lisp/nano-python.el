;;; nano-python.el --- Python language programming environment

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

;; Configures programming environment for the Python language.

;;; Code:
(require 'eglot)
(require 'poetry)
(require 'python)


(defun nano-setup-python-with-eglot ()
  "Setup and enable Eglot for Python.
For additional settings please refer to Pyright documentation
https://github.com/microsoft/pyright/blob/main/docs/configuration.md"

  (poetry-tracking-mode)

  ;; Allow other checkers
  (setq flycheck-eglot-exclusive nil)

  (eglot-ensure))

(with-eval-after-load "python"
  (setq poetry-tracking-strategy 'switch-buffer)

  ;; Configure linting
  (add-to-list 'flycheck-checkers 'python-flycheck)
  (add-to-list 'flycheck-checkers 'python-mypy)

  (add-hook 'python-ts-mode-hook #'nano-setup-python-with-eglot))

;; Let Emacs guess Python indent silently
(setq python-indent-guess-indent-offset t
      python-indent-guess-indent-offset-verbose nil)

;; Enable syntax highlighting for Python-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.coveragerc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.importlinter\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("poetry.lock\\'" . conf-toml-mode))

(provide 'nano-python)
;;; nano-python.el ends here
