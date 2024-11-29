;;; nano-json.el --- Environment for JSON configuration format -*- lexical-binding: t; -*-

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
(defun nano-setup-json-mode ()
  "Setup markdown mode."
  (add-hook 'flymake-diagnostic-functions #'flymake-collection-jsonlint nil t))

(with-eval-after-load 'json-ts-mode
  ;; Show indentation
  (add-hook 'json-ts-mode-hook #'indent-bars-mode)

  (add-hook 'json-ts-mode-hook #'nano-setup-json-mode))

(add-to-list 'auto-mode-alist '("\\.json\\." . json-ts-mode))

(provide 'nano-json)
;;; nano-json.el ends here
