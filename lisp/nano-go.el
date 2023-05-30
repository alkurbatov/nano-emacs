;;; nano-go.el --- Golang programming environemnt

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

(defun nano-setup-go-with-lsp ()
  "Setup and enable lsp-mode for Go."

  ;; Disable lsp checker in favor of golangci-lint.
  ;; This should be set before invocation of lsp (lsp-deferred) command.
  (setq-local lsp-diagnostics-provider :none)

  (lsp-deferred)

  (setq lsp-go-use-placeholders nil)

  ;; Set up before-save hooks to format buffer on save and add/delete imports.
  ;; Make sure no other gofmt/goimports hooks enabled.
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(with-eval-after-load "go-mode"
  (require 'flycheck-golangci-lint)
  (require 'lsp)

  (setq flycheck-golangci-lint-tests t)
  (setq flycheck-golangci-lint-fast t)

  (define-key go-mode-map (kbd "M-.") #'godoc-at-point)

  (add-hook 'go-mode-hook 'flycheck-golangci-lint-setup)
  (add-hook 'go-mode-hook 'nano-setup-go-with-lsp)
  (add-hook 'go-mode-hook 'go-eldoc-setup)
  (add-hook 'go-mode-hook 'tree-sitter-hl-mode)

  ;; Enable syntax highlight in godoc buffer.
  (add-hook 'godoc-mode-hook 'go-mode))

(provide 'nano-go)
;;; nano-go.el ends here
