;;; nano-go.el --- Golang programming environment -*- lexical-binding: t; -*-

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
(require 'bind-key)
(require 'format-all)
(require 'project)

;; Make it possible for LSP servers to find go.mod in the project.
;; See https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)


;; Add gofumpt formatter
(define-format-all-formatter gofumpt
  (:executable "gofumpt")
  (:install "go install mvdan.cc/gofumpt@latest")
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable)))

;; Add gci formatter
(define-format-all-formatter gci
  (:executable "gci")
  (:install "go install github.com/daixiang0/gci@latest")
  (:languages "Go")
  (:features)
  (:format (format-all--buffer-easy executable "print")))

(defun nano-setup-go-with-eglot ()
  "Setup and enable Eglot for Go."
  ;; Ask Eglot to stay away from completely taking over flymake
  (setq eglot-stay-out-of '(flymake))
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend)

  ;; Configure linting
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-golangci-lint nil t)

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-workspace-configuration
                 '((:gopls . ((:usePlaceholders . t))))))

  (eglot-ensure))

(with-eval-after-load 'go-ts-mode
  ;; Sync indent with global settings
  (setq go-ts-mode-indent-offset 4)

  ;; Show indentation
  (add-hook 'go-ts-mode-hook #'indent-bars-mode)

  (add-hook 'go-ts-mode-hook #'nano-setup-go-with-eglot))

;; Enable syntax highlighting for Golang-related tools configuration files
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mod-ts-mode))

(provide 'nano-go)
;;; nano-go.el ends here
