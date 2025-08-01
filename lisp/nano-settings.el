;;; nano-settings.el --- Customizable project settings -*- lexical-binding: t; -*-

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
(defvar os-linux (string-equal system-type "gnu/linux")
  "Running on Linux.")

(defvar os-macos (string-equal system-type "darwin")
  "Running on macOS.")

(defcustom nano-brew-path "/opt/homebrew"
  "Set homebrew directory. Should be set to '/usr/local' for non-silicon Macs."
  :type 'string
  :group 'nano)

(defcustom nano-rfc-directory "~/work/rfc/"
  "Path to the directory containing RFCs downloaded by the rfc-mode package."
  :type 'string
  :group 'nano)

(defcustom nano-lock-files-directory (concat user-emacs-directory "locks/")
  "Where Emacs should store lock files."
  :type 'string
  :group 'nano)

(defcustom nano-auto-save-directory (concat user-emacs-directory "auto-save-list/")
  "Where Emacs should store auto saved files."
  :type 'string
  :group 'nano)

(defcustom nano-backup-directory (concat user-emacs-directory "backups/")
  "Where Emacs should store file backups."
  :type 'string
  :group 'nano)

(defcustom nano-font-height 130
  "The default font height (in 1/10th points)."
  :type 'number
  :group 'nano)

(provide 'nano-settings)
;;; nano-settings.el ends here
