;;; nano-markdown.el --- Markdown language configuration

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
(require 'thingatpt)

(defun flyspell-ignore-markdown-links ()
  (not (thing-at-point 'url)))

(defun nano-setup-markdown-mode ()
  "Setup markdown mode with spellcheck."

  (setq markdown-command "pandoc")

  ;; Add spell checking ignore rules
  (put 'markdown-mode 'flyspell-mode-predicate 'flyspell-ignore-markdown-links)

  (flyspell-mode)

  (unless (string= (buffer-name) "COMMIT_EDITMSG")
    (flyspell-buffer)))

(with-eval-after-load "markdown-mode"
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'markdown-mode-hook #'nano-setup-markdown-mode)) ; immediately check whole buffer

(provide 'nano-markdown)
;;; nano-markdown.el ends here
