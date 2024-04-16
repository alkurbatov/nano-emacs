;;; nano-markdown.el --- Markdown language configuration -*- lexical-binding: t; -*-

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
(defun nano-setup-markdown-mode ()
  "Setup markdown mode with spellcheck."

  (setq markdown-command "pandoc")

  ;; Configure linting
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-markdownlint nil t)
  (flyspell-mode)

  (setq-local whitespace-style '(face trailing))
  (whitespace-mode)

  (unless (string= (buffer-name) "COMMIT_EDITMSG")
    (flyspell-buffer)))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook #'nano-setup-markdown-mode)) ; immediately check whole buffer

;; Use Github Markdown flavor by default (provided by markdown-mode) as it is used most often
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

(provide 'nano-markdown)
;;; nano-markdown.el ends here
