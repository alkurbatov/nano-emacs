;;; nano-sh.el --- Bash/Sh programming environment -*- lexical-binding: t; -*-

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

;; Configures programming environment for Bash/Sh scripts.

;;; Code:

(defun nano-setup-sh-with-eglot ()
  "Setup and enable Eglot for Bash/Sh."
  ;; Ask Eglot to stay away from completely taking over flymake
  (setq eglot-stay-out-of '(flymake))
  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend)

  ;; Regarding settings see:
  ;; Eglot: https://www.gnu.org/software/emacs/manual/html_node/eglot/User_002dspecific-configuration.html
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '((sh-mode bash-ts-mode) . ("bash-language-server" "start"))))

  (eglot-ensure))

(add-hook 'bash-ts-mode-hook #'nano-setup-sh-with-eglot)

;; Enable tree-sitter integration.
(add-to-list 'treesit-auto-langs 'bash)

;; Enable syntax highlighting for Sh-related files
(add-to-list 'auto-mode-alist '("\\.env\\'" . bash-ts-mode))

(provide 'nano-sh)
;;; nano-sh.el ends here
