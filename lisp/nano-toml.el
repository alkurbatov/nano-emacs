;;; nano-toml.el --- Environment for TOML configuration format

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

;; Configures environment for the TOML configuration format

;;; Code:

(with-eval-after-load "conf-toml-mode"
  (add-hook 'conf-toml-mode-hook #'display-line-numbers-mode))

(provide 'nano-toml)
;;; nano-toml.el ends here
