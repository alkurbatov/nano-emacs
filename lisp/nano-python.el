;;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; Configures programming environment for the Python language.
;; ---------------------------------------------------------------------

(defun setup-python-with-lsp ()
  "Setup and enable lsp-mode for Python."
  (poetry-tracking-mode)

  ;; Disable lsp checker in favor of flake8+mypy.
  ;; This should be set before invocation of lsp (lsp-deferred) command.
  (setq-local lsp-diagnostics-provider :none)

  (lsp-deferred)

  (flycheck-add-next-checker 'lsp 'python-flake8)
  (flycheck-add-next-checker 'python-flake8 'python-mypy))

(use-package python
  :ensure nil

  :config
  (use-package poetry
    :config
    (setq poetry-tracking-strategy 'switch-buffer))

  (use-package python-black)
  (use-package py-isort)

  (use-package lsp-pyright
    :custom
    ;; We use mypy instead.
    (lsp-pyright-typechecking-mode "off"))

  ;; Suppress annoing indent guessing messages
  (setq python-indent-guess-indent-offset-verbose nil)

  ;; Configure linting
  (add-to-list 'flycheck-checkers 'python-flycheck)
  (add-to-list 'flycheck-checkers 'python-mypy)

  :hook
  ((python-mode . setup-python-with-lsp))
   (python-mode . tree-sitter-hl-mode))

;; Enable syntax highlighting for particular configuration files
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . conf-mode))

(provide 'nano-python)
