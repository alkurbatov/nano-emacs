;;; nano-yaml.el --- YAML format support

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

(defun override-colors ()
  "Override faces for better syntax highlighting.
We have to do it here because changes should be applied only to the yaml mode."
  (face-remap-add-relative 'tree-sitter-hl-face:variable
                           '(:foreground "#8FBCBB")) ; frost-0
  (face-remap-add-relative 'font-lock-keyword-face:variable
                           '(:foreground "#8FBCBB"))) ; frost-0

(add-hook 'yaml-mode-hook #'tree-sitter-hl-mode)
(add-hook 'yaml-mode-hook #'override-colors)

(provide 'nano-yaml)
;;; nano-yaml.el ends here
