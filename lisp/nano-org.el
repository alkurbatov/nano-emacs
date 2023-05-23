;;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; Configures programming environment for the Python language.
;;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; Configure Org mode.
;; ---------------------------------------------------------------------

(defcustom nano-font-family-monospaced "JetBrains Mono"
  "Name of the font-family to use for nano.
Defaults to JetBrains Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'nano
  :type 'string)

(defcustom nano-org-directory "~/Yandex.Disk.localized/org/Проекты"
  "Name of the directory containing Org files with TOOD items)."
  :type 'string
  :group 'nano)

(use-package org
  :ensure nil

  :config
  ;; Small speedup, we don't need the agenda after every start
  (setq org-agenda-inhibit-startup t)

  ;; Load org files with tasks
  (setq org-agenda-files (directory-files-recursively nano-org-directory "\\.org$"))

  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE" "CANCELLED")))
    
  ;; Add current time when marking item as 'DONE'
  (setq org-log-done 'time)

  :bind
  (("C-c a" . org-agenda)))

(provide 'nano-org)
