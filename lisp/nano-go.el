;;; nano-go.el --- Golang programming environment -*- lexical-binding: t; -*-

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
(require 'bind-key)
(require 'format-all)
(require 'gotest-ts)

;; Register gofumpt formatter.
(define-format-all-formatter gofumpt
                             (:executable "gofumpt")
                             (:install "go install mvdan.cc/gofumpt@latest")
                             (:languages "Go")
                             (:features)
                             (:format (format-all--buffer-easy executable)))

;; Register gci formatter.
(define-format-all-formatter gci
                             (:executable "gci")
                             (:install "go install github.com/daixiang0/gci@latest")
                             (:languages "Go")
                             (:features)
                             (:format (format-all--buffer-easy executable "print")))

;; Register golangci-lint formatter.
(define-format-all-formatter golangci-lint
                             (:executable "golangci-lint")
                             (:install "See https://golangci-lint.run/docs/welcome/install/")
                             (:languages "Go")
                             (:features)
                             (:format (format-all--buffer-easy executable "fmt" "--stdin")))

(defun nano-setup-go-with-eglot ()
  "Setup and enable Eglot for Go."
  ;; Ask Eglot to stay away from completely taking over flymake
  (setq eglot-stay-out-of '(flymake))
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend)

  ;; Configure linting
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-golangci-lint nil t)

  ;; Regarding settings see:
  ;; Eglot: https://www.gnu.org/software/emacs/manual/html_node/eglot/User_002dspecific-configuration.html
  ;; Gopls: https://github.com/golang/tools/blob/master/gopls/doc/settings.md
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `(go-ts-mode . ("gopls" :initializationOptions
                                 (:hints (:constantValues t
                                                          :compositeLiteralFields t
                                                          :functionTypeParameters t
                                                          :parameterNames t))))))

  (eglot-ensure)

  ;; Disable annoying action indicators.
  ;; By some reason this variable is always overloaded after call to eglot-ensure.
  (setq eglot-code-action-indications nil))

(with-eval-after-load 'go-ts-mode
  ;; Sync indent with global settings
  (setq go-ts-mode-indent-offset 4)

  ;; Make tests execution with gotest more verbose
  (setq go-test-verbose t)

  ;; Show indentation
  (add-hook 'go-ts-mode-hook #'indent-bars-mode)

  ;; Treat camel case substrings as words.
  (add-hook 'go-ts-mode-hook #'subword-mode)

  (add-hook 'go-ts-mode-hook #'nano-setup-go-with-eglot)

  (bind-key "<f2>" #'gotest-ts-run-dwim))

;; Enable tree-sitter integration.
(add-to-list 'treesit-auto-langs 'go)
(add-to-list 'treesit-auto-langs 'gomod)

;; Enable syntax highlighting for Golang-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(provide 'nano-go)
;;; nano-go.el ends here
