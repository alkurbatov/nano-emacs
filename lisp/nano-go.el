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
(require 'bind-key)
(require 'flycheck-golangci-lint)
(require 'lsp)


(defun nano-setup-go-with-lsp ()
  "Setup and enable lsp-mode for Go."

  (setq flycheck-golangci-lint-tests t)
  (setq flycheck-golangci-lint-fast t)
  (flycheck-golangci-lint-setup)

  ;; Disable lsp checker in favor of golangci-lint.
  ;; This should be set before invocation of lsp (lsp-deferred) command.
  (setq-local lsp-diagnostics-provider :none)

  (lsp-deferred)

  (setq lsp-go-use-placeholders nil))

(add-hook 'go-ts-mode-hook #'nano-setup-go-with-lsp)

;; Enable syntax highlighting for Golang-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(provide 'nano-go)
;;; nano-go.el ends here
