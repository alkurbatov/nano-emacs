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
(require 'python)

(defun nano-setup-python-with-lsp ()
  "Setup and enable lsp-mode for Python."

  (poetry-tracking-mode)

  ;; Disable lsp checker in favor of flake8+mypy.
  ;; This should be set before invocation of lsp (lsp-deferred) command.
  (setq-local lsp-diagnostics-provider :none)

  (lsp-deferred)

  ;; Use mypy instead of pyright
  (customize-set-variable 'lsp-pyright-typechecking-mode "off")

  (flycheck-add-next-checker 'lsp 'python-flake8)
  (flycheck-add-next-checker 'python-flake8 'python-mypy))

(with-eval-after-load "python"
  (require 'lsp)
  (require 'lsp-pyright)
  (require 'poetry)

  (setq poetry-tracking-strategy 'switch-buffer)


  ;; Let Emacs guess Python indent silently
  (setq python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil)

  ;; Configure linting
  (add-to-list 'flycheck-checkers 'python-flycheck)
  (add-to-list 'flycheck-checkers 'python-mypy)

  (add-hook 'python-mode-hook #'nano-setup-python-with-lsp)
  (add-hook 'python-mode-hook #'tree-sitter-hl-mode))

;; Enable syntax highlighting for Python-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.coveragerc\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.flake8\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("poetry.lock\\'" . conf-toml-mode))

(provide 'nano-python)
;;; nano-python.el ends here
