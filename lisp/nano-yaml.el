;;; nano-yaml.el --- YAML language configuration

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

(with-eval-after-load 'yaml-ts-mode
  ;; Show indentation
  (add-hook 'yaml-ts-mode-hook #'highlight-indent-guides-mode)

  (add-hook 'yaml-ts-mode-hook #'turn-on-smartparens-mode)
  (add-hook 'yaml-ts-mode-hook #'ethan-wspace-mode))

(provide 'nano-yaml)
;;; nano-yaml.el ends here
