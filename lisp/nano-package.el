;;; nano-package.el --- Integration of package management -*- lexical-binding: t; -*-

;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2023-2025 - N Λ N O developers

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

;; Install straight package manager
;; See https://github.com/radian-software/straight.el

;;; Commentary:

;;; Code:

;; Implicitly adds :straight t to all use-package forms
(setq straight-use-package-by-default t)

;; Speed up packages downloads by checking out only current branch instead of the default 'full
(setq straight-vc-git-default-clone-depth '(1 single-branch))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Enable upgrade of builtin packages,
;; see https://irreal.org/blog/?p=12409
(setq package-install-upgrade-built-in t)

;; Useful to know how much time packages are taking.
(setq use-package-compute-statistics t)

;; Require newest org as soon as possible to avoid loading of the old version.
(straight-use-package 'org)

(provide 'nano-package)
;;; nano-package.el ends here
