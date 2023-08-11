;;; nano-help.el --- Help and embedded documentation improvements

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
(require 'engine-mode)

;; Binded to 'C-x /' by default
(engine-mode t)

(defengine pypi
           "https://pypi.org/search/?q=%s"
           :keybinding "y")

(defengine github
           "https://github.com/search?q=%s&type=code"
           :keybinding "h")

(provide 'nano-help)
;;; nano-help.el ends here
