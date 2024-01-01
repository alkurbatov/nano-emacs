;;; nano-settings.el --- Customizable project settings

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
(defcustom nano-brew-path "/opt/homebrew"
  "Set homebrew directory.  Should be set to '/usr/local' for non-silicon Macs."
  :type 'string
  :group 'nano)

(defcustom nano-org-directory "~/Yandex.Disk.localized/org/Проекты"
  "Name of the directory containing Org files with TODO items)."
  :type 'string
  :group 'nano)

(provide 'nano-settings)
;;; nano-settings.el ends here
